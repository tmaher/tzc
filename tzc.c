/*
   tzc.c  trivial zephyr client (once upon a time)
   Copyright (C) 1992, 1993 Scott Draves (spot@cs.cmu.edu)
   Copyright (C) 1994, 1995, 1996, 1998 Darrell Kindred (dkindred@cs.cmu.edu)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

   Now at version 2.6.16

   Thanks to Nick Thompson and Darrell Kindred for their contributions.

   this program is a replacement for zwgc and zwrite that talks to emacs
   in printed sexps.  Contact dkindred@cs.cmu.edu to get zephyr.el.

   Darrell Kindred <dkindred@cs.cmu.edu> is the current tzc
   maintainer.

   TODO

   - write man page
   - make check() print an error msg that zephyr.el will see, rather than
     the silent "; <error>" variety.
   - see about incorporating poole's setlocation change, activated
     by some cmd-line flag
   - allow emacs to tag outgoing zgrams with some identifier, which
     will be used in the (tzcspew . sent), (tzcspew . not-sent)
     etc. messages referring to that zgram.  This would, in principle,
     allow zephyr.el to do something more reasonable with the acks for
     multi-recipient zgrams.
   - [very old] if you get a USR1 in the middle of writing out a msg, then
     the output will not be properly formatted.  so should either
     disable the sig for this time, or set a flag in the handler and
     check at the main loop.  the same probably applies to the alarm.


   CHANGES

   10 Dec 2004  (ver 2.6.16) added opcode and zlocate support - aarons@aberrant.org
   15 Jun 2000  (ver 2.6.15) handle malformed (< 0) z_time.tv_usec
   30 May 2000  (ver 2.6.14) handle malformed (> 999999) z_time.tv_usec
   30 May 2000  (ver 2.6.13) change z_default_format (include time/date etc.)
   06 Mar 2000  (ver 2.6.12) support get-location
   05 Mar 2000  (ver 2.6.10) support set-location
   05 Mar 2000  (ver 2.6.04) com_err reports errors via elisp
   07 Dec 1998  (ver 2.6.03) more typechecking in send_zgram()
   16 Oct 1998  (ver 2.6.02) print feature list in startup msg
   16 Oct 1998  (ver 2.6.01) allow modifying/removing queries
   15 Oct 1998  (ver 2.6.00) support `register-query'
   29 Sep 1998  (ver 2.5.04) allow sender to be specified for outgoing z's
                             (code from poole/scottd)
   23 Sep 1998  (ver 2.5.03) replace bcopy and friends with ANSI mem*
   19 Sep 1998  (ver 2.5.02) fix fputqqs to handle symbols looking like numbers
   17 Sep 1998  (ver 2.5.01) fix gcc warnings, lread.c supports \NNN escapes
   27 Jul 1998  remove pointless forced-termination message
   14 Jan 1997  (version 2.5) support 'ayt' command
   6  Jun 1996  added #include <errno.h> for EWOULDBLOCK
   14 Mar 1994  (version 2.4) cross-realm support (#ifdef INTERREALM),
                added subscribe command (see zsubscribe), 
                added (tzcspew . start) message on startup
   18 Feb 1994  (version 2.3) provide heartbeat feature to combat
                the problem of zephyr servers which forget you exist
   12 Feb 1994  (version 2.2) provide -o flag for output-only, and
                print a machine-readable timestamp ('time-secs field)
		in addition to the 'time string.
   26 Sep 1993  print more usage info,
                exit gracefully upon EOF on stdin or socket
   9  Apr 1993  (version 2.1)
                on exit or restart: cancel subscriptions, unset
		  location, remove the -p pid-file.
		set location (for zlocate) to tzc.pid (by setting DISPLAY)
		added -l flag to explicitly set the location
		added version number to greeting
   18 Jan 1993  received -a option from hsw@cs.cmu.edu, it restarts the
                process if no messages have been received after arg seconds.
   6  Jan 1993  added p: to options string, i forgot it in the merge
   1 Jan  1993  replace ((tzcspew . sent) (to "spot"))
                with    ((tzcspew . sent) (to "PERSONAL" . "spot"))
   20 Dec 1992  removed silly old code that was forcing recip to be "" for
                non-personal instances.
   sometime     recognizes (auth . nil) to disable kerberos authentication
   16 Dec 1992  added some more error checking to send_zgram so that it
                handles garbled input without dying.
   13 Dec 1992  merged in lisp reading stuff.  no more calls to zwrite.
   25 Oct 1992  if we get a USR1 signal, then restart ourselves. added -p
                flag to write our pid into a file. this is so that kauthd
	        users can restart tzc automatically when they get new
	        tickets.
   28 Jul 1992  added wait() to subscribe_with_zctl() to prevent zombies.

   
  */

#define TZC_VERSION "2.6.16"

#include <stdarg.h>
#include <string.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <ctype.h>
#include <zephyr/zephyr.h>
#include <zephyr/zephyr_err.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <sys/wait.h>
#include <sys/file.h>
#include <signal.h>
#include <time.h>
#include <sys/time.h>
#include <fcntl.h>
#include <errno.h>
#include "lread.h"
#include <et/com_err.h>
#if USE_LIBCS
#include <libcs.h>	      /* for snprintf */
#endif
#if defined(sgi)
/* FD_ZERO needs this.  Same problem under AIX but no bstring.h exists. */
#include <bstring.h>
#endif

#ifdef INTERREALM
extern char *ZExpandRealm();
#endif

#define ZCTL_BINARY "zctl"

#define TZC_HEARTBEAT_MESSAGE  "tzc\000thump thump"
#define TZC_HEARTBEAT_CLASS    "TZC"
#define TZC_HEARTBEAT_INSTANCE "heartbeat"
#define HEARTBEAT_PERIOD_DEF   5 * 60      /* sent hearbeat after 5 minutes */
#define HEARTBEAT_TIMEOUT      45	   /* timeout if it doesn't arrive
					    * within 45 seconds */

/* these should eventually be set to 0, maybe in 2.6 */
#define TZC_CLASS_IS_SYMBOL 1
#define TZC_OPCODE_IS_SYMBOL 1

/* include kosak query-processing code */
/* #define QUERY_HELL 1 */

#if __hpux__
#define random lrand48
#define srandom srand48
#endif

#if QUERY_HELL
void *HellStartup();
void  HellShutdown(void *hellCookie);
void *HellCompileQuery (void *hellCookie, const char *queryp);
void  HellDeleteQuery (void *hellCookie, void *compiled_query);
int   HellEvaluateQuery(void *hellCookie, void *queryCookie, 
			const char *zgram, int length);
#endif

typedef struct PendingReply PendingReply;
struct PendingReply {
   char *instance;
   char *recipient;
   ZUnique_Id_t	uid;
   PendingReply *next;
};

#if QUERY_HELL
typedef struct RegisteredQuery_s {
   char *id;
   char *raw_query;
   void *compiled_query;
   struct RegisteredQuery_s *next;
} RegisteredQuery;
#endif

struct Globals {
   const char	*program;
   int          argc;
   char         **argv;

   u_short	port;
   int		use_stdin;	/* if 0, don't listen to stdin at all */
   int          ignore_eof;     /* if 1, ignore eof on stdin */
   int		stdin_flags;
   int		zfd;
   char *	pidfile;
#if 0
   char *	my_zephyr_id;	/* the user's zephyr "name" */
   char *	my_realm;	/* the user's realm */
#endif
   char *       exposure;
   char *       location;
   char *       hostname;
   int		debug;
   int          default_sub;    /* if 0, don't sub to -c MESSAGE */

   struct {
       enum {
           HB_DISABLED, /* don't use heartbeats */
	   HB_ENABLED,  /* use heartbeat -- we're alive */
	   HB_TESTING,  /* use heartbeat -- hb sent, waiting for it to come */
	   HB_DEAD      /* use heartbeat -- server seems to have dropped us */
       } status;
       long timeout;    /* if the heartbeat doesn't arrive in this number of
			 * seconds, send the (tzcspew . cutoff) to emacs */
       long wakeup;     /* if not 0, time at which we should wake up
 		         * to see if the server has forgotten us */
       long period;     /* if no zgrams have arrived in this number of seconds,
			 * send a heartbeat */
   } heartbeat;

#if QUERY_HELL
   RegisteredQuery *queries;
   void *query_hell_cookie;
#endif

   int		ebufsiz;
   char *	ebuf;
   char *	ebufptr;

   ZAsyncLocateData_t locate_data;
   /* linked list of messages sent which are waiting for replies */
   PendingReply *pending_replies;

   struct {
      Value *sym_class;
      Value *sym_instance;
      Value *sym_sender;
      Value *sym_recipients;
      Value *sym_message;
      Value *sym_send;
      Value *sym_tzcfodder;
      Value *sym_auth;
      Value *sym_subscribe;
      Value *sym_get_location;
      Value *sym_set_location;
      Value *sym_location;
      Value *sym_exposure;
      Value *sym_hostname;
      Value *sym_ayt;
      Value *sym_register_query;
      Value *sym_query;
      Value *sym_id;
      Value *sym_opcode;
      Value *sym_zlocate;
   } constants;
};

struct Globals global_storage, *globals;

void usage() {
   fprintf(stderr, "usage: %s [options]\n", globals->program);
   fprintf(stderr, "   options:\n");
   fprintf(stderr, "      -a <nseconds>  restart tzc every <nseconds> seconds\n");
   fprintf(stderr, "      -e <exposure>  set exposure (values: NONE, OPSTAFF, REALM-VISIBLE,\n");
   fprintf(stderr, "                     REALM-ANNOUNCED, NET-VISIBLE, NET-ANNOUNCED)\n");
   fprintf(stderr, "      -l <location>  set zlocation tty to the given string\n");
   fprintf(stderr, "                     (default: tzc.n, where n is tzc's pid)\n");
   fprintf(stderr, "      -h <hostname>  set zlocation hostname to the given string\n");
   fprintf(stderr, "                     (default: current hostname)\n");   
   fprintf(stderr, "      -p <filename>  write tzc's process-id to the indicated file\n");
   fprintf(stderr, "      -s             use zctl for subscriptions (read from ~/.zephyr.subs.tzc)\n");
   fprintf(stderr, "      -t <nseconds>  if no zgrams arrive in <nseconds> seconds, send a test\n");
   fprintf(stderr, "                     msg to make sure we're alive.  Default 300, 0 disables.\n");
   fprintf(stderr, "      -o             output-only (just print zgrams, don't listen to stdin)\n");
   /* it's really time to move to long option names... */
   fprintf(stderr, "      -i             ignore eof on input\n");
   fprintf(stderr, "      -d             print debugging information\n");
}

/* warning: this uses ctime which returns a pointer to a static buffer
 * which is overwritten with each call. */
char *time_str(time_t time_num)
{
    char *now_name;
    now_name = ctime(&time_num);
    now_name[24] = '\0';	/* dump newline at end */
    return(now_name);
}

/* return time in the format "14:15:03" */
/* uses ctime, which returns a ptr to a static buffer */
char *debug_time_str(time_t time_num)
{
    char *now_name;
    now_name = ctime(&time_num);
    now_name[19] = '\0';	/* strip year */
    return now_name+11;		/* strip date */
}

#if DEBUG_LOG
void debug_log(char *s)
{
  char fname[50];
  FILE *f;
  if (globals->debug) {
    sprintf(fname, "/tmp/tzc-log.%ld", (long) getpid());
    if ((f = fopen(fname,"a")) != NULL) {
      fprintf(f, "[%s] %s\n", debug_time_str(time(NULL)), s);
      fclose(f);
    }
  }
}
#else  /* !DEBUG_LOG */
#define debug_log(s) ((void)0)
#endif

/* I'm paranoid about leaving stdin non-blocking, though I doubt it's a */
/* problem in this case. - nix */
void bail(int code) {
   debug_log("bail: start");
   if (globals->use_stdin) {
      if (-1 == fcntl(0, F_SETFL, globals->stdin_flags)) {
         perror("unable to reset stdin");
         code = 1;
      }
   }
   debug_log("bail: done");
   exit(code);
}

void unblock_signals()
{
#if USE_SIGSETMASK
   int mask = sigsetmask(0);
   (void) sigsetmask(mask & ~sigmask(SIGUSR1) & ~sigmask(SIGALRM));
#else
   sigset_t sigs;
   sigemptyset(&sigs);
   sigaddset(&sigs, SIGUSR1);
   sigaddset(&sigs, SIGALRM);
   sigprocmask(SIG_UNBLOCK, &sigs, NULL);
#endif
}

void
sign_off(msg)
char *msg;
{
        debug_log("sign_off()");

	if (globals->pidfile != NULL)
		unlink(globals->pidfile);

        ZCancelSubscriptions(0);
        if (globals->exposure != NULL)
		ZUnsetLocation();
	ZClosePort();

	printf("\n; tzc %s\n;;;\n",msg);
}

void restart_tzc(int sig) {
   unblock_signals();
   sign_off("restarting");
   execvp(globals->argv[0], globals->argv);
}

void kill_tzc(int sig) {
   unblock_signals();
   sign_off("killed");
   bail(1);
}

void exit_tzc() {
   /* graceful exit, no error */
   debug_log("exit_tzc()");
#if QUERY_HELL
   if (globals->query_hell_cookie) {
     HellShutdown(globals->query_hell_cookie);
     globals->query_hell_cookie = 0;
     debug_log("exit_tzc(): hellshutdown done");
   }
#endif
   unblock_signals();
   debug_log("exit_tzc(): signals unblocked");
   sign_off("exiting");
   debug_log("exit_tzc(): signed off");
   bail(0);
}		 

Code_t check(Code_t e, char *s) {
   if (e) {
      printf(";;; return code %d\n",(int) e);
      fflush(stdout);
      com_err(__FILE__, e, s);
      bail(1);
   }
   return e;
}

Code_t warn(Code_t e, char *s) {
   if (e)
      com_err(__FILE__, e, s);
   return e;
}

/* useful for debugging */
void list_subs() {
   int i, n, one = 1;
   ZSubscription_t sub;
   ZRetrieveSubscriptions(0, &n);
   printf(";;; subscriptions for %s:\n", ZGetSender());
   for (i = 0; i < n; i++) {
      ZGetSubscriptions(&sub, &one);
      printf(";;;    recip \"%s\", class \"%s\", inst \"%s\"\n",
#if (ZVERSIONMAJOR > 0) || (ZVERSIONMINOR >= 2)
	     sub.zsub_recipient, sub.zsub_class, sub.zsub_classinst
#else
	     sub.recipient, sub.class, sub.classinst
#endif
	     );
   }
}


void subscribe() {
   Code_t retval = ZERR_NONE;
   int retry;
   static ZSubscription_t sub[] =
   {/* recipient, class, instance -- wildcard not allowed on class */
    {"%me%", "MESSAGE", "*"},
    {"*", TZC_HEARTBEAT_CLASS, TZC_HEARTBEAT_INSTANCE}, //, if default message,*,* sub
/*    {"%me%", TZC_HEARTBEAT_CLASS, TZC_HEARTBEAT_INSTANCE}, */
#ifdef TEST_XREALM
    {"*@CS612.CMU.EDU", "MESSAGE", "*"},
    {"*@AGRABAH.CS.CMU.EDU", "MESSAGE", "*"},
    {"*@CS.CMU.EDU", "MESSAGE", "*"},
    {"*@ANDREW.CMU.EDU", "MESSAGE", "*"},
#endif
#if DK_TEST_EXTRA_CLASSES
    {"*", "GLOBAL", "*"},
    {"*", "CMU", "*"},
#endif
};
   int n = sizeof(sub) / sizeof(ZSubscription_t);
   static ZSubscription_t default_sub[] = {
     {"*", "MESSAGE", "*"}
};
   int default_n = sizeof(sub) / sizeof(ZSubscription_t);
#if (ZVERSIONMAJOR > 0) || (ZVERSIONMINOR >= 2)
   sub[0].zsub_recipient = ZGetSender();
#else
   sub[0].recipient = ZGetSender();
#endif
   /* sometimes we get a SERVNAK while subscribing, for no apparent reason.
    * so we'll try up to three times */
   for (retry = 0; retry < 3; retry++) {
      if (retry>0) {
         sleep(2);
         printf("; retrying...\n");
	 fflush(stdout);
      }
      /* The cast is necessary for the alpha, because zephyr.h assumes
       * sizeof(int) == sizeof(long) */
      if ((retval = ZSubscribeTo(sub, n, 0)) != (Code_t) ZERR_SERVNAK)
         break;
      printf("; SERVNAK received while attempting to subscribe\n");
      fflush(stdout);
   }
   check(retval, "ZSubscribeTo");
   if (globals->default_sub) {
     for (retry = 0; retry < 3; retry++) {
       if (retry>0) {
	 sleep(2);
	 printf("; retrying...\n");
	 fflush(stdout);
       }
       /* The cast is necessary for the alpha, because zephyr.h assumes
	* sizeof(int) == sizeof(long) */
       if ((retval = ZSubscribeTo(default_sub, default_n, 0)) != (Code_t) ZERR_SERVNAK)
	 break;
       printf("; SERVNAK received while attempting to subscribe\n");
       fflush(stdout);
     }
     check(retval, "ZSubscribeTo");
   }
}

void subscribe_with_zctl() {
  int pid;
  FILE *fd;
  char st[1024];
  char setwgfile[1200];
  char *portfile;
  extern char *getenv();

  portfile = getenv("WGFILE");

  if(!portfile) {
    sprintf(st,"/tmp/wg.%ld",(long) getuid());
    portfile = st;
    sprintf(setwgfile,"WGFILE=%s",st);
    if (putenv(setwgfile) != 0) {
	fprintf(stderr,"putenv(\"%s\") failed.",setwgfile);
	bail(1);
    }
  }

  fd = fopen(portfile,"w");
  if(!fd) {
    perror(portfile);
    bail(1);
  }
  fprintf(fd, "%d\n", globals->port);
  fclose(fd);

  if(!(pid = fork())) {
      char fn[1024];
      sprintf(fn,"%s/.zephyr.subs.tzc",getenv("HOME"));
      execlp(ZCTL_BINARY,"zctl","load",fn,NULL);
      perror("zctl exec");
      fprintf(stderr,"Unable to load subscriptions.\n");
      bail(0);
    }
  if(pid == -1) {
      perror("zctl exec");
      fprintf(stderr,"Unable to fork and load subscriptions.\n");
    }
  while (pid != wait(0));
}


/*
  like fputs, but quotes the string as necc for 
  reading as a string by gnu-emacs
  */
void fputqs(const char *s, FILE *o) {
   char c;
   putc('\"', o);
   while ((c = *s++)) {
      if (c == '\1') {
	 putc('\\', o);
	 putc('1', o);
#if 0  /* don't want to activate this until i know all the tzc-output parsers
        * out there can handle \t */
      } else if (c == '\t') {
	 /* apparently some zephyr-remote variants don't handle tabs well */
         putc('\\', o);
         putc('t', o);
#endif
      } else {
	 if (c == '\"' || c == '\\')
	    putc('\\', o);
	 putc(c, o);
      }
   }
   putc('\"', o);
}

/* like fputs, but quotes for reading as a symbol by gnu-emacs */
void fputqqs(const char *s, FILE *o) {
   char c;
   if ('\0' == *s) {
      fputs("nil", o);
   } else {
      /* these goofy (int) casts prevent gcc warnings under dux40 */
      if (isdigit((int) *s) || *s == '+' || *s == '-')
	 putc('\\', o);	      /* avoid number/symbol confusion */
      while ((c = *s++)) {
	 if (!(isalnum((int) c) || strchr("-+*/_~!@$%^&=:<>{}", c)))
	    putc('\\', o);
	 putc(c, o);
      }
   }
}

void
emacs_put_open()
{
   fputs("\001(", stdout);
}

void
emacs_put_close()
{
   putc(')', stdout);
   putc('\0', stdout);
   putc('\n', stdout);
   fflush(stdout);
}

void
emacs_put_pair_open(const char *tag)
{
   putc('(', stdout);
   fputqqs(tag, stdout);
   fputs(" . ", stdout);
}

void
emacs_put_pair_close()
{
   fputs(") ", stdout);
}

void
emacs_put_sym(const char *tag, char *sym)
{
   putc('(', stdout);
   fputqqs(tag, stdout);
   fputs(" . ", stdout);
   if (sym[0])
      fputqqs(sym, stdout);
   else
      fputs("nil", stdout);
   fputs(") ", stdout);
}

void
emacs_put_str(const char *tag, const char *str)
{
   putc('(', stdout);
   fputqqs(tag, stdout);
   fputs(" . ", stdout);
   fputqs(str, stdout);
   fputs(") ", stdout);
}

void
emacs_put_int(const char *tag, int i)
{
   putc('(', stdout);
   fputqqs(tag, stdout);
   fputs(" . ", stdout);
   printf("%d", i);
   fputs(") ", stdout);
}

void
emacs_put_str_str(const char *tag, const char *a, const char *b)
{
   putc('(', stdout);
   fputqqs(tag, stdout);
   putc(' ', stdout);
   fputqs(a, stdout);
   fputs(" . ", stdout);
   fputqs(b, stdout);
   fputs(") ", stdout);
}   

void
emacs_error(char *err)
{
   static int reentry = 0;
   if (reentry) {
      /* die a horrible death */
      printf("Looping emacs_error.  Aieee!  I perish in flames!\n");
      exit(5);
   }
   reentry = 1;
   emacs_put_open();
   emacs_put_sym("tzcspew", "error");
   emacs_put_str("message", err);
   emacs_put_close();
   reentry = 0;
}

static void tzc_com_err_hook(const char *whoami, long errcode, 
			     const char *fmt, va_list ap)
{
    char buf1[4096], errmsg[4096];

    vsnprintf(buf1, sizeof(buf1), fmt, ap);
    snprintf(errmsg, sizeof(errmsg), "%s: %s %s", 
	     whoami, error_message(errcode), buf1);
    emacs_error(errmsg);
}

char *auth_string(int n) {
   switch (n) {
    case ZAUTH_YES    : return "yes";
    case ZAUTH_FAILED : return "failed";
    case ZAUTH_NO     : return "no";
    default           : return "bad-auth-value";
   }
}
 
char *kind_string(int n) {
   switch (n) {
    case UNSAFE:    return "unsafe";
    case UNACKED:   return "unacked";
    case ACKED:     return "acked";
    case HMACK:     return "hmack";
    case HMCTL:     return "hmctl";
    case SERVACK:   return "servack";
    case SERVNAK:   return "servnak";
    case CLIENTACK: return "clientack";
    case STAT:      return "stat";
    default:        return "bad-kind-value";
   }
}

/* Send a zgram with the specified parameters.
 * Note: the instance and recipient passed in will be free()'d when 
 *       the reply comes back. 
 * Returns zero on success, non-zero if there's a failure sending
 * (recipient not logged in or not subscribing is *not* a "failure sending").
 */
int
send_zgram_to_one(char *class, char *opcode, char *sender,
		  char *instance, char *recipient, 
		  char *message, int message_len, int (*auth)())
{
   ZNotice_t notice;
   int retval;
   char bfr[BUFSIZ];

   memset(&notice, '\0', sizeof(notice));

   notice.z_kind = ACKED;
   notice.z_port = 0;
   notice.z_class = class;
   notice.z_opcode = opcode;
   notice.z_sender = sender;
   notice.z_class_inst = instance;
   notice.z_recipient = recipient;
   notice.z_message = message;
   notice.z_message_len = message_len;
   if (auth == ZAUTH) {
      notice.z_default_format = "Class $class, Instance $instance:\nTo: @bold($recipient) at $time $date\nFrom: @bold($1) <$sender>\n\n$2";
   } else {
      notice.z_default_format = "@bold(UNAUTHENTIC) Class $class, Instance $instance:\nTo: @bold($recipient) at $time $date\nFrom: @bold($1) <$sender>\n\n$2";
   }
   if ((retval = ZSendNotice(&notice, auth)) != ZERR_NONE) {
      (void) sprintf(bfr, "while sending notice to %s", 
		     notice.z_recipient);
      com_err(__FILE__, retval, bfr);
      /* XXX should probably free instance & recipient here */
      return 1;
   } else {
      /* prepare for reply */
      PendingReply *reply = (PendingReply *) malloc(sizeof(PendingReply));
      reply->instance = notice.z_class_inst; /* freed when reply comes back */
      reply->recipient = notice.z_recipient; /* freed when reply comes back */
      reply->uid = notice.z_uid;
      /* any other info to save? */
      reply->next = globals->pending_replies;
      globals->pending_replies = reply;
      return 0;
   }
}

void
send_zgram(Value *spec)
{
    int (*auth)();
    Value *v;
    Value *recip_list;
    Value *message_list;
    int message_len;
    char *message = 0, *class, *instance, *sender, *recipient, *opcode;

    /* emacs sends something of the form:
     * ((class . "MESSAGE") 
     *  (auth . t)
     *  (recipients ("PERSONAL" . "bovik") ("test" . ""))
     *  (sender . "bovik")
     *  (message . ("Harry Bovik" "my zgram"))
     * )
     */
    /* pull class, sender, auth, recip_list, and message of spec */
    /* class */
    v = assqv(globals->constants.sym_class, spec);
    if (VTAG(v) != cons || VTAG(VCDR(v)) != string) {
       emacs_error("class not defined");
       goto fail;
    }
    class = vextract_string_c(VCDR(v));
    /* opcode */
    v = assqv(globals->constants.sym_opcode, spec);
    if (VTAG(v) != cons) /* ? */
      opcode = "";
    else
      opcode = vextract_string_c(VCDR(v));
    /* recipients */
    v = assqv(globals->constants.sym_recipients, spec);
    if (VTAG(v) != cons) {
       emacs_error("recipients not defined");
       goto fail;
    }
    recip_list = VCDR(v);

    /* sender */
    v = assqv(globals->constants.sym_sender, spec);
    sender = ((VTAG(v) != cons || (VTAG(VCDR(v)) != string)) ? 0 
	      : vextract_string_c(VCDR(v)));

    /* message */
    v = assqv(globals->constants.sym_message, spec);
    if (VTAG(v) != cons) {
       emacs_error("message not defined");
       goto fail;
    }
    message_list = VCDR(v);

    /* auth */
    auth = ZAUTH;
    v = assqv(globals->constants.sym_auth, spec);
    if (VTAG(v) == cons &&
	VTAG(VCDR(v)) == nil)
       auth = ZNOAUTH;

    if (1) {
       /* cdr through the message list, determining total length */
       Value *chase_message_list = message_list;
       char *buffer_mark;
       message_len = 0;
       while (VTAG(chase_message_list) == cons) {
	  Value *one_field = VCAR(chase_message_list);
	  if (VTAG(one_field) != string) {
	    emacs_error("bad (non-string) message component");
	    goto bailout;
	  }
	  message_len += VSLENGTH(one_field) + 1;
	  chase_message_list = VCDR(chase_message_list);
       }

       buffer_mark = message = (char *) malloc(message_len);
       
       /* cdr through again, this time copying strings into the buffer */
       chase_message_list = message_list;
       while (VTAG(chase_message_list) == cons) {
	  Value *one_field = VCAR(chase_message_list);
	  memcpy(buffer_mark, VSDATA(one_field), VSLENGTH(one_field));
	  buffer_mark += VSLENGTH(one_field);
	  *buffer_mark++ = 0;
	  chase_message_list = VCDR(chase_message_list);
       }
    }
    
    /* cdr through the recipient list */
    while (VTAG(recip_list) == cons) {
       Value *recip_pair = VCAR(recip_list);

       if (VTAG(recip_pair) != cons
	   || VTAG(VCAR(recip_pair)) != string
	   || VTAG(VCDR(recip_pair)) != string) {
	  emacs_error("bad recipient");
	  goto bailout;
       }

       /* these strings are freed when the reply comes back in */
       instance = vextract_string_c(VCAR(recip_pair));
       recipient = vextract_string_c(VCDR(recip_pair));
#if 0
#ifdef INTERREALM
       {
	  /* check for an instance with an @ in it */
	  char *realm, *newrecip, *rlm = rindex(instance, '@');

	  if (rlm) {
	     *rlm++ = '\0';
	     realm = ZExpandRealm(rlm);
	     newrecip = (char *) malloc(strlen(recipient)+1+strlen(realm)+1);
	     sprintf(newrecip,"%s@%s",recipient,rlm);
	     free(recipient);
	     recipient = newrecip;
	  }
       }
#endif /* INTERREALM */
#endif

       if (send_zgram_to_one(class, opcode, sender, instance, recipient,
			     message, message_len, auth) != 0)
	  break;
       recip_list = VCDR(recip_list);
    }

  bailout:
  fail:
    if (message) free(message);
}

/* emacs can send ((tzcfodder . ayt)) and tzc will respond with 
 * ((tzcspew . ayt-response))
 */
void
handle_ayt(Value *ayt_cmd)
{
   emacs_put_open();
   emacs_put_sym("tzcspew", "ayt-response");
   emacs_put_close();
}

#if QUERY_HELL
/* return 0 on success */
static int
insert_query(char *id, char *query)
{
  void *compiled_query;

  if ((globals->query_hell_cookie == NULL)
      && ((globals->query_hell_cookie = HellStartup()) == NULL)) {
    /* errmsg = "can't initialize query support"; */
    return 1;
  }
  
  if ((compiled_query = HellCompileQuery(globals->query_hell_cookie,
					 query)) == NULL) {
    /* errmsg = "invalid query"; */
    return 1;
  }

  {
    RegisteredQuery *newq = malloc(sizeof(RegisteredQuery));
    if (newq == NULL) { 
      fprintf(stderr,"tzc.c: out of memory\n");
      bail(1);
    }
    newq->id = strdup(id);
    newq->raw_query = strdup(query);
    newq->compiled_query = compiled_query;
    newq->next = globals->queries;
    globals->queries = newq;
  }
  return 0;
}

static void
free_query(RegisteredQuery *q)
{
  if (q) {
    if (q->id) 
      free(q->id);
    if (q->raw_query) 
      free(q->raw_query);
    if (q->compiled_query)
      HellDeleteQuery(globals->query_hell_cookie, q->compiled_query);
    free(q);
  }
}

/* return 1 if query was found */
static int
remove_query(char *id)
{
  RegisteredQuery **prev, *q;
  for (prev = &globals->queries, q = globals->queries; q != NULL; q=q->next) {
    if (!strcmp(id, q->id)) {
      *prev = q->next;
      free_query(q);
      return 1;
    }
  }
  return 0;
}
#endif /* QUERY_HELL */

void
register_query(Value *register_query_cmd)
{
  char *query = 0, *id = 0;
  char *errmsg = 0;

#if QUERY_HELL
  while (VTAG(register_query_cmd) == cons) {
    Value *pair = VCAR(register_query_cmd);
    Value *tag, *val;
    register_query_cmd = VCDR(register_query_cmd);

    if (VTAG(pair) != cons) {
      errmsg = "bad command format";
      goto done;
    }
    tag = VCAR(pair);
    val = VCDR(pair);
    if (eqv(tag, globals->constants.sym_query) && VTAG(val) == string) {
      query = vextract_string_c(val);
    } else if (eqv(tag, globals->constants.sym_id) && VTAG(val) == symbol) {
      id = vextract_string_c(val);
    } else {
      /* ignore tzcfodder and unknown tags */
    }
  }
  if (id == NULL) {
    errmsg = "id missing or not a symbol";
    goto done;
  }

  /* put this query in the globals->queries linked list */
  (void) remove_query(id);
  if (query && insert_query(id,query) != 0)
    errmsg = "bad query";

#else /* not QUERY_HELL */
  errmsg = "queries not supported";
  goto done;
#endif

done:
  emacs_put_open();
  emacs_put_sym("tzcspew", "register-query-response");
  if (id) emacs_put_sym("id", id);
  if (query) emacs_put_str("query", query);
  if (errmsg) {
    emacs_put_sym("status", "nil");
    emacs_put_str("message", errmsg);
  } else {
    emacs_put_sym("status", "t");
  }
  emacs_put_close();

  if (id) free(id);
  if (query) free(query);
}

void
handle_zlocate(Value *cmd)
{
   Value *cur;
   char *cur_userid;
   
   cmd = VCDR(cmd); /* chop the fodder */

   while (VTAG(cmd) == cons)
   {
      cur = VCAR(cmd);
      cmd = VCDR(cmd);
      /* for each userid */

      if (VTAG(cur) != string) {
	 emacs_error("non-string zlocate parameter, ignoring");
	 continue;
      }

      cur_userid = vextract_string_c(cur);

      check(ZRequestLocations(cur_userid, &globals->locate_data, UNSAFE, ZNOAUTH), "ZRequestLocations");
      /* send a request string.. */
   }
}

void
zsubscribe(Value *subscribe_cmd)
{
   ZSubscription_t *zsubs;
   Value *subs_list, *sub;
   int i, nsubs;
   Value *recip_v, *class_v, *inst_v;
   
   /* emacs sends subscriptions in this form:
    * ((tzcfodder . subscribe)
    *  (class instance recip) (class instance recip) ... ))
    */
   subs_list = VCDR(subscribe_cmd);   /* strip off tzcfodder */

   nsubs = vlength(subs_list);
   zsubs = (ZSubscription_t *) calloc(nsubs,sizeof(ZSubscription_t));
   
   for (i=0; i<nsubs; i++, subs_list = VCDR(subs_list)) {
      sub = VCAR(subs_list);
      if (vlength(sub) != 3) {
	 emacs_error("bad subscription format (sub must have 3 parts)");
	 goto fail;
      }
      class_v = VCAR(sub);
      inst_v = VCAR(VCDR(sub));
      recip_v  = VCAR(VCDR(VCDR(sub)));
      if (VTAG(recip_v) != string ||
	  VTAG(class_v) != string ||
	  VTAG(inst_v) != string) {
	 emacs_error("bad subscription format (non-string)");
	 goto fail;
      }
#if (ZVERSIONMAJOR > 0) || (ZVERSIONMINOR >= 2)
      zsubs[i].zsub_recipient = vextract_string_c(recip_v);
      zsubs[i].zsub_class = vextract_string_c(class_v);
      zsubs[i].zsub_classinst  = vextract_string_c(inst_v);
#else
      zsubs[i].recipient = vextract_string_c(recip_v);
      zsubs[i].class = vextract_string_c(class_v);
      zsubs[i].classinst  = vextract_string_c(inst_v);
#endif
   }
   if (VTAG(subs_list) != nil) {
      emacs_error("bad subscription format");
      goto fail;
   }
   check(ZSubscribeTo(zsubs, nsubs, 0), "ZSubscribeTo");
   emacs_put_open();
   emacs_put_sym("tzcspew", "subscribed");
   emacs_put_close();
 fail:
   for (i--; i>=0; i--) {
#if (ZVERSIONMAJOR > 0) || (ZVERSIONMINOR >= 2)
      free(zsubs[i].zsub_recipient);
      free(zsubs[i].zsub_class);
      free(zsubs[i].zsub_classinst);
#else
      free(zsubs[i].recipient);
      free(zsubs[i].class);
      free(zsubs[i].classinst);
#endif
   }
   free(zsubs);
}

static int
set_location() {
   ZUnsetLocation();	      /* no error checking here */
   if (globals->exposure != NULL) {
#if 1
     int rc = check(ZInitLocationInfo(globals->hostname, globals->location), 
		    "ZInitLocationInfo");
     if (rc != ZERR_NONE) {
       return rc;
     }
#else
     /* old libzephyr didn't support ZInitLocationInfo, so we had to	
      * resort to this: */
      char *setdisplay = malloc(strlen("DISPLAY=")
				+ strlen(globals->location)
				+ 1);
      if (setdisplay == NULL) {
         fprintf(stderr, "tzc.c: out of memory\n");
	 bail(1);
      }
      sprintf(setdisplay, "DISPLAY=%s", globals->location);
      if (putenv(setdisplay) != 0) {
         fprintf(stderr, "putenv(\"%s\") failed.", setdisplay);
	 bail(1);
      }
#endif
      return check(ZSetLocation(globals->exposure), "ZSetLocation");
   } else {
     return ZERR_NONE;
   }
}

static void
report_location()
{
   emacs_put_open();
   emacs_put_sym("tzcspew", "location");
   emacs_put_str("hostname",
		 globals->hostname == NULL ? "" : globals->hostname);
   emacs_put_str("location", globals->location);
   emacs_put_str("exposure", 
		 globals->exposure == NULL ? "NONE" : globals->exposure);
   emacs_put_close();
}

static void
handle_get_location(Value *getloc_cmd)
{
    report_location();
}

static void
handle_set_location(Value *setloc_cmd)
{
   char *new_exposure = NULL, *new_location = NULL, *new_hostname = NULL;
   int new_exposure_given = 0, new_location_given = 0, new_hostname_given = 0;
   Value *exposure_v, *location_v, *hostname_v;
   /* emacs sends subscriptions in this form:
    * ((tzcfodder . set-location)
    *    (exposure . "NET-ANNOUNCED")     ; optional - default is current exp.
    *                                     ; (value nil means no exposure)
    *    (location . "tzc.123")           ; optional - default is current loc
    *  )
    * a nil or non-existent value is interpreted as no-exposure
    */
   setloc_cmd = VCDR(setloc_cmd);   /* strip off tzcfodder */
   exposure_v = assqv(globals->constants.sym_exposure, setloc_cmd);
   location_v = assqv(globals->constants.sym_location, setloc_cmd);
   hostname_v = assqv(globals->constants.sym_hostname, setloc_cmd);

   if (exposure_v != NULL) {
     Value *exposure_str = VCDR(exposure_v);
     if (exposure_str == NULL) {
       new_exposure_given = 1;
       new_exposure = NULL;
     } else if (VTAG(exposure_str) == string) {
       new_exposure_given = 1;
       if (0 == strcasecmp(globals->exposure, "NONE")) {
	 new_exposure = NULL;
       } else {
	 new_exposure = vextract_string_c(exposure_str);
       }
     } else {
       emacs_error("bad exposure argument in set-location");
       goto fail;
     }
   }

   if (location_v != NULL) {
     Value *location_str = VCDR(location_v);
     if (location_str != NULL
	 && VTAG(location_str) == string) {
       new_location_given = 1;
       new_location = vextract_string_c(location_str);
     } else {
       emacs_error("bad location argument in set-location");
       goto fail;
     }
   }

   if (hostname_v != NULL) {
     Value *hostname_str = VCDR(hostname_v);
     if (hostname_str != NULL
	 && VTAG(hostname_str) == string) {
       new_hostname_given = 1;
       new_hostname = vextract_string_c(hostname_str);
     } else {
       emacs_error("bad hostname argument in set-location");
       goto fail;
     }
   }      
   
   if (new_hostname_given) {
      free(globals->hostname);
      globals->hostname = new_hostname;
   }

   if (new_location_given) {
      free(globals->location);
      globals->location = new_location;
   }
   if (new_exposure_given) {
     if (globals->exposure) 
       free(globals->exposure);
     globals->exposure = new_exposure;
   }

   if (new_location_given || new_exposure_given || new_hostname_given) {
     if (set_location() != ZERR_NONE) {
       return;
     }
   }
   report_location();

 fail:
   /* nothing to do */
   return;
}


void
process_sexp(Value *v)
{
   Value *pair;
   Value *key;

#if 0
   printf("read: ");
   prin(stdout, v);
   fputc('\n', stdout);
#endif

   if (VTAG(v) == cons &&
       NULL != (pair = assqv(globals->constants.sym_tzcfodder, v)) &&
       NULL != (key = VCDR(pair))) {
      if (eqv(key, globals->constants.sym_send))
	 send_zgram(v);
      else if (eqv(key, globals->constants.sym_subscribe))
	 zsubscribe(v);
      else if (eqv(key, globals->constants.sym_set_location))
	 handle_set_location(v);
      else if (eqv(key, globals->constants.sym_get_location))
	 handle_get_location(v);
      else if (eqv(key, globals->constants.sym_ayt))
	 handle_ayt(v);
      else if (eqv(key, globals->constants.sym_register_query))
	 register_query(v);
      else if (eqv(key, globals->constants.sym_zlocate))
	handle_zlocate(v);
      else
	{
	  char *err = malloc(VSLENGTH(key) + 30);
	  char *key_str = vextract_string_c(key);
	  if (err && key) {
	    sprintf(err, "bad tzcfodder key: `%s'", key_str);
	    emacs_error(err);
	    free(err);
	    free(key_str);
	  } else {
	    fprintf(stderr,"tzc.c: out of memory\n");
	    bail(1);
	  }
	}
   } else {
      emacs_error("no tzcfodder key");
   }

#if 0
	 printf("parsed: ");
	 prin(stdout, v);
	 fputc('\n', stdout);
	 if (destructure(pattern, v)) {
	    printf("match_data = ");
	    prin(stdout, match_data);
	    fputc('\n', stdout);
	 }
	 else {
	    printf("destructure failed\n");
	 }
#endif
}

int
read_emacs()
{
   int ret;
   Value *v;
   
   /* if the buffer is full, expand it*/
   if (globals->ebufptr - globals->ebuf >= globals->ebufsiz) {
      char *new_buf;
      globals->ebufsiz *= 2;
      new_buf = (char *) malloc(globals->ebufsiz);
      memcpy(new_buf, globals->ebuf, globals->ebufptr - globals->ebuf);
      globals->ebufptr = new_buf + (globals->ebufptr - globals->ebuf);
      free(globals->ebuf);
      globals->ebuf = new_buf;
   }

   /* read up to size of buffer */
   ret = read(0, globals->ebufptr,
	      globals->ebufsiz - (globals->ebufptr - globals->ebuf));
   if (ret == 0) {
      return -1;
   } else if (ret < 0) {
      if (errno != EWOULDBLOCK) {
	 perror("reading from emacs");
	 bail(1);
      }
   } else {
      globals->ebufptr += ret;
   }

   /* parse & process all available input from emacs */
   do {
#if 1
#ifndef MIN
#define MIN(a,b) ((a)<(b) ? (a) : (b))
#endif
      if (globals->debug) {
	int len = globals->ebufptr - globals->ebuf;
	char thestr[4096];
	strncpy(thestr,globals->ebuf,MIN(len,4096));
	thestr[len] = '\0';
	if (thestr[len-1] == '\n')
	  thestr[len-1] = '\0';
	printf(";;; parsing string [[[%s]]]\n", thestr);
      }
#endif
      ret = parse(globals->ebufptr - globals->ebuf,
		  globals->ebuf, &v);
#if 0
      printf("parse returned %d\n",ret);
#endif
      if (ret > 0) {
	 memcpy(globals->ebuf, globals->ebuf + ret,
	       (globals->ebufptr - globals->ebuf) - ret);
	 globals->ebufptr -= ret;

	 process_sexp(v);

	 free_value(v);
      }
   } while (ret > 0);
   return 0;
}

void say_hi() {
   printf("; tzc is free software and you are welcome to distribute copies\n");
   printf("; of it under certain conditions; see the source code for details.\n");
   printf("; Version %s\n",TZC_VERSION);
   printf("; Copyright 1992, 1993 Scott Draves\n");
   printf("; Copyright 1994, 1995, 1996, 1998, 2000 Darrell Kindred\n");
   printf("; Started: %s\n\n", time_str(time(0)));
   fflush(stdout);
}

void reset_heartbeat() {
   int fuzz;
   if (globals->heartbeat.status != HB_DISABLED) {
     globals->heartbeat.status = HB_ENABLED;
     /* add +/- 20% random fuzz to the period to prevent everyone from 
      * pounding the server at once */
     fuzz = globals->heartbeat.period / 5;
     globals->heartbeat.wakeup = time(0) + globals->heartbeat.period + 
       (random() % 1000 - 500) * fuzz / 500;
     if (globals->debug) {
       printf(";;; %s ",debug_time_str(time(0)));
       printf("reset heartbeat (will wake up at %s)\n",
	      debug_time_str(globals->heartbeat.wakeup));
       fflush(stdout);
     }
   }
}

void
setup(int use_zctl)
{
   /* set stdin up for nonblocking reads, saving previous flags so we can */
   /* restore them later */
   if (globals->use_stdin) {
      if (-1 == (globals->stdin_flags = fcntl(0, F_GETFL, 0))) {
         perror("fcntl(0, F_GETFL)");
         exit(1);
      }
      if (-1 == (fcntl(0, F_SETFL, globals->stdin_flags | FNDELAY))) {
         perror("fcntl(0, F_SETFL)");
         bail(1);
      }
   }

   /* if there were an easy way to block-buffer stdout
      i'd do it here */

   /* have com_err send elisp errors rather than just printing to stderr */
   set_com_err_hook(tzc_com_err_hook);

   check(ZInitialize(), "ZInitialize");
   globals->port = 0;
   check(ZOpenPort(&globals->port), "ZOpenPort");

   if ((globals->zfd= ZGetFD()) < 0) {
      perror("ZGetFD");
      bail(1);
   }

   if (use_zctl)
      subscribe_with_zctl();
   else
      subscribe();
   if (globals->debug) {
      printf(";;; %s done with zsubscribe\n",debug_time_str(time(0)));
      list_subs();
      fflush(stdout);
   }

#if QUERY_HELL
   globals->queries = NULL;
   globals->query_hell_cookie = NULL;
#endif

   globals->ebufsiz = 256;
   globals->ebuf = (char *) malloc(globals->ebufsiz);
   globals->ebufptr = globals->ebuf;

   reset_heartbeat();

   globals->pending_replies = NULL;

   globals->constants.sym_class = vmake_symbol_c("class");
   globals->constants.sym_instance = vmake_symbol_c("instance");
   globals->constants.sym_recipients = vmake_symbol_c("recipients");
   globals->constants.sym_sender = vmake_symbol_c("sender");
   globals->constants.sym_message = vmake_symbol_c("message");
   globals->constants.sym_send = vmake_symbol_c("send");
   globals->constants.sym_tzcfodder = vmake_symbol_c("tzcfodder");
   globals->constants.sym_auth = vmake_symbol_c("auth");
   globals->constants.sym_subscribe = vmake_symbol_c("subscribe");
   globals->constants.sym_get_location = vmake_symbol_c("get-location");
   globals->constants.sym_set_location = vmake_symbol_c("set-location");
   globals->constants.sym_exposure = vmake_symbol_c("exposure");
   globals->constants.sym_location = vmake_symbol_c("location");
   globals->constants.sym_hostname = vmake_symbol_c("hostname");
   globals->constants.sym_ayt = vmake_symbol_c("ayt");
   globals->constants.sym_register_query = vmake_symbol_c("register-query");
   globals->constants.sym_query = vmake_symbol_c("query");
   globals->constants.sym_id = vmake_symbol_c("id");
   globals->constants.sym_opcode = vmake_symbol_c("opcode");
   globals->constants.sym_zlocate = vmake_symbol_c("zlocate");
}

void
await_input(int *emacs_in, int *zephyr_in)
{
   struct timeval t, *timeout;
   fd_set fdset;

   *emacs_in = *zephyr_in = 0;

   if (ZPending() > 0) {
      *zephyr_in = 1;
      return;
   }

   FD_ZERO(&fdset);
   if (globals->use_stdin)
      FD_SET(0, &fdset);		/* stdin from emacs */
   FD_SET(globals->zfd, &fdset);	/* Zephyr incoming */

   if (globals->heartbeat.status == HB_ENABLED ||
       globals->heartbeat.status == HB_TESTING) {
#if 0
      /* If we print stuff here, zephyr.el can get confused and think
       * we've printed an error message because of the
       *  (if (not (equal log-buffer-size (buffer-size)))
       * test in zephyr-send. */
      if (globals->debug) {
	 printf(";;; %s doing select,", debug_time_str(time(0)));
	 printf(" will wake up at %s (%d secs)\n", 
		debug_time_str(globals->heartbeat.wakeup),
		globals->heartbeat.wakeup - time(0));
	 fflush(stdout);
      }
#endif
      /* add 2s just to be sure we don't wake up too soon. */
      t.tv_sec = globals->heartbeat.wakeup - time(0) + 2;
      t.tv_usec = 0;
      timeout = &t;
   } else {
      timeout = NULL;
   }
   
   if (select(globals->zfd + 1, &fdset, NULL, NULL, timeout) < 0) {
      perror("select");
   } else {
      *emacs_in = globals->use_stdin ? FD_ISSET(0, &fdset) : 0;
      *zephyr_in = FD_ISSET(globals->zfd, &fdset);
      /* this didn't work - we'll just assume that a whole packet
       * will arrive soon after anything shows up on the input
       if (*zephyr_in)
       if (warn(ZPending(), "ZPending") < 1)
       *zephyr_in = 0;
       */
   }
}

#if QUERY_HELL
/* This produces the following format (from the zarchive server spec):
     Each zephyrgram consists of these
     seven fields in the given order, each terminated by an ASCII 0 (NUL):
        sender, signature, timestring, instance, body, timesecs, archived
     The whole zephyrgram is terminated by an ASCII 1 (^A).
   The "archived" flag is always set to 1.
   The returned value should be freed by the caller.
 */
int zgram_to_zarchive_format (ZNotice_t *notice, char **zgram) 
{
  int total_len;
  char *time_string = time_str(notice->z_time.tv_sec);
  char time_secs[20];
  char *archived = "1";
  char *sig = "", *body = "";
  int sig_len, body_len;

  sprintf(time_secs, "%d", notice->z_time.tv_sec);

  for (sig_len = 0; sig_len < notice->z_message_len; sig_len++)
    if (notice->z_message[sig_len] == '\0')
      break;
  sig = notice->z_message;

  for (body_len=0; sig_len + 1 + body_len < notice->z_message_len; body_len++)
    if (notice->z_message[sig_len + 1 + body_len] == '\0') 
      break;
  body = notice->z_message + sig_len + 1;

  total_len = (strlen(notice->z_sender)       + 1
	       + sig_len                      + 1
	       + strlen(time_string)          + 1
	       + strlen(notice->z_class_inst) + 1
	       + body_len                     + 1
	       + strlen(time_secs)	      + 1
	       + strlen(archived)             + 1
	       + 1  /* for the \001 */
	       );

  if ((*zgram = malloc(total_len)) == NULL) {
    fprintf(stderr,"tzc.c: out of memory\n");
    bail(1);
  }
  {
    char *p = *zgram;
    memset(p, 0, total_len);

    strcpy(p, notice->z_sender);
    p += strlen(notice->z_sender) + 1;

    strncpy(p, sig, sig_len);
    p += sig_len + 1;

    strcpy(p, time_string);
    p += strlen(time_string) + 1;
    
    strcpy(p, notice->z_class_inst);
    p += strlen(notice->z_class_inst) + 1;

    strncpy(p, body, body_len);
    p += body_len + 1; 

    strcpy(p, time_secs);
    p += strlen(time_secs) + 1;

    strcpy(p, archived);
    p += strlen(archived) + 1;

    *p++ = '\001';

    if (p - *zgram != total_len) {
      fprintf(stderr,"tzc.c: internal bug #24601 (%d/%d)\n",
	      (int) (p - *zgram), total_len);
      bail(1);
    }
  }

  return total_len;
}

/* this will print (query . ((q1 . nil) (q2 . t) (q3 . t)))
 * to indicate the queries matched and not matched by this zgram
 */
void 
check_queries(ZNotice_t *notice) {
  RegisteredQuery *q = globals->queries;
  if (q) {
    char *zgram;
    int zgram_len = zgram_to_zarchive_format (notice, &zgram);

    /* XXX debug */
    emacs_put_int("encoded-zgram-len", zgram_len);
#if 0
    printf("\nzgram: ");
    fwrite(zgram, zgram_len, 1, stdout);
    printf("\n");
#endif      
    emacs_put_pair_open("queries");
    putc('(', stdout);
    while (q) {
      int match = HellEvaluateQuery(globals->query_hell_cookie, 
				    q->compiled_query,
				    zgram, zgram_len);
      emacs_put_sym(q->id, match ? "t" : "nil");
      q = q->next;
    }
    putc(')', stdout);
    emacs_put_pair_close();
  }
}
#endif /* QUERY_HELL */

void
report_locate_notice(ZNotice_t *notice)
{
   int numlocs;
   char *user;
   int one = 1;
   ZLocations_t locations;
   
   if (check(ZParseLocations(notice, (ZAsyncLocateData_t *)NULL,
			     &numlocs, &user), "ZParseLocations")
       != ZERR_NONE)
      return;

   if (globals->debug)
       printf(";;; numlocs = %d\n", numlocs);
   emacs_put_open();
   emacs_put_sym("tzcspew", "zlocation");

   emacs_put_pair_open("time-secs");
   fprintf(stdout, "(%d %d %d)", 
	   (int) (notice->z_time.tv_sec >> 16),
	   (int) (notice->z_time.tv_sec & 0xffff), 
	   (int) notice->z_time.tv_usec);
   emacs_put_pair_close();

   emacs_put_str("user", user);
   
   emacs_put_pair_open("locations");
   fputs("(", stdout);
   for (;numlocs;numlocs--) {
      if (check(ZGetLocations(&locations,&one), "ZGetLocations") != ZERR_NONE)
      {
	 emacs_put_pair_close();
	 emacs_put_close();
	 return;
      }
      
      fputs("(", stdout);
      emacs_put_str("host", locations.host);
      emacs_put_str("tty", locations.tty);
      emacs_put_str("time", locations.time);
      fputs(") ", stdout);
   }
   fputs(")", stdout);
   emacs_put_pair_close();
   emacs_put_close();
}

void
report_zgram(ZNotice_t *notice, int auth)
{
      int i, len, forced_termination;
      const char *from_host;
      char *p, *instance, *recipient;
      struct hostent *hst;
      int lag = 0, lag_valid = 0;

      /* convert IP# of sender into ascii name */
      hst = gethostbyaddr((char *)&notice->z_sender_addr,
			  sizeof(notice->z_sender_addr), AF_INET);
      from_host = hst ? hst->h_name : inet_ntoa(notice->z_sender_addr);

      /* abbreviate sender by throwing away realm if it's local */
      p = strchr(notice->z_sender, '@');
      if (p && !strcmp(p+1, ZGetRealm()))
	 *p = '\0';

      /* ditto for recipient */
      p = strchr(notice->z_recipient, '@');
      if (p && !strcmp(p+1, ZGetRealm()))
	 *p = '\0';

      /* Class is supposed to be case-insensitive, so we could
       * convert to upper-case here, but we'll leave it to emacs
       * to decide what to do.  It's a bit inconvenient that we
       * send class as a symbol rather than a string, but emacs
       * can use symbol-name and then convert to uppercase. */
#if 0
      for (p = notice->z_class; *p != '\0'; p++) {
         if (islower(*p))
            (*p) = toupper(*p);
      }
#endif

      if (globals->debug) {
	 if (globals->heartbeat.status == HB_TESTING) {
	    lag = HEARTBEAT_TIMEOUT - (globals->heartbeat.wakeup - time(0));
	    lag_valid = 1;
	 } else {
	    lag_valid = 0;
         }
      }

      /* zephyr server is still talking to us, so reset the heartbeat */
      reset_heartbeat();

      /* if this is a heartbeat zgram, don't report it */
      if (!strcasecmp(notice->z_class, TZC_HEARTBEAT_CLASS) &&
	  !strcasecmp(notice->z_class_inst, TZC_HEARTBEAT_INSTANCE)) {
	 if (globals->debug) {
	    printf(";;; %s received heartbeat zgram from %s on %s",
		   debug_time_str(time(0)),
		   notice->z_sender, from_host);
	    if (lag_valid) {
		    printf(" after %d seconds",lag);
	    }
	    printf("\n");
	    fflush(stdout);
	 }
         return;
      }


      if (!strcasecmp(notice->z_opcode, "LOCATE") &&
	  !strcasecmp(notice->z_class, "USER_LOCATE"))
      {
	 report_locate_notice(notice);
	 return;
      }

      /* XXX one of these days we need to check return values from all
       * these mallocs */
      /* XXX can we assume that z_recipient and z_class_inst are not NULL? */
#ifdef INTERREALM
#if 0
      /* if recipient is "@SOMEREALM" then use empty recipient & 
       * add "@SOMEREALM" to the instance */
      if (notice->z_recipient[0]=='@') {
	 instance = (char *) malloc(strlen(notice->z_class_inst)+
				    strlen(notice->z_recipient)+1);
	 strcpy(instance, notice->z_class_inst);
	 strcat(instance, notice->z_recipient);
	 recipient = (char *) malloc(1);
	 recipient[0] = '\0';
      } else
#endif
#endif /* INTERREALM */
      {
	 instance = (char *) malloc(strlen(notice->z_class_inst)+1);
	 strcpy(instance, notice->z_class_inst);
	 recipient = (char *) malloc(strlen(notice->z_recipient)+1);
	 strcpy(recipient, notice->z_recipient);
      }

      emacs_put_open();
      emacs_put_sym("tzcspew", "message");
      emacs_put_sym("kind", kind_string(notice->z_kind));

#if QUERY_HELL
      check_queries(notice);
#endif

#if TZC_CLASS_IS_SYMBOL
      emacs_put_sym
#else
      emacs_put_str
#endif
	           ("class", notice->z_class);
      emacs_put_str("instance", instance);
#if TZC_OPCODE_IS_SYMBOL
      emacs_put_sym
#else
      emacs_put_str
#endif
                   ("opcode", notice->z_opcode);
      emacs_put_str("sender", notice->z_sender);
      emacs_put_str("recipient", recipient);
      emacs_put_int("port", notice->z_port);
      emacs_put_sym("auth", auth_string(auth));
      emacs_put_str("time", time_str(notice->z_time.tv_sec));
      if (notice->z_time.tv_usec > 999999
	  || notice->z_time.tv_usec < 0) {
	  char buf[80];
	  sprintf(buf, "tv_sec=%lu tv_usec=%lu", 
		  (unsigned long) notice->z_time.tv_sec,
		  (unsigned long) notice->z_time.tv_usec);
	  emacs_put_str("malformed-time-stamp", buf);
          notice->z_time.tv_usec = 0;
      }
      emacs_put_pair_open("time-secs");
      fprintf(stdout, "(%d %d %d)", 
	      (int) (notice->z_time.tv_sec >> 16),
	      (int) (notice->z_time.tv_sec & 0xffff), 
	      (int) notice->z_time.tv_usec);
      emacs_put_pair_close();
#if 1
/* dsk### just for timing tests */
      {
	      struct timeval tv;
	      double latency;
	      gettimeofday(&tv, NULL);
	      latency = tv.tv_sec - notice->z_time.tv_sec +
		        (tv.tv_usec - notice->z_time.tv_usec) / 1000000.0;
	      emacs_put_pair_open("latency");
	      fprintf(stdout, "\"%.2f\"", latency);
	      emacs_put_pair_close();
      }
#endif
      emacs_put_str("fromhost", from_host);

      emacs_put_pair_open("message");
      putc('(', stdout);

      p = notice->z_message;
      len = notice->z_message_len;
      forced_termination = 0;
      
      if (p[len-1]) {
	 /* force null termination */
	 p = (char *) malloc(len+1);
	 memcpy(p, notice->z_message, len);
	 p[len] = '\0';
	 len += 1;
	 forced_termination = 1;
      }
      for (i = 0; i < len; i++) {
#if 0
      {
	 FILE *f = fopen("/tmp/blah","a");
	 fprintf(f,"[[message part: %s]]\n",p+i);
	 fclose(f);
      }		
#endif
	 fputqs(p+i, stdout);
	 putc(' ', stdout);
	 i += strlen(p+i);
      }
      putc(')', stdout);
      emacs_put_pair_close();

      if (forced_termination) {
 	 /* no real need to report this.
	    emacs_put_sym("forced-termination", "t"); */
	 free(p);
      }

      emacs_put_close();

      free(instance);
      free(recipient);

#if 0
      fputs("(kind . ", stdout);
      fputs(kind_string(notice->z_kind), stdout);
      fputs(") (class . ", stdout);
      fputqqs(notice->z_class, stdout);
      fputs(") (instance . ", stdout);
      fputqs(notice->z_class_inst, stdout);
      fputs(") (opcode . ", stdout);
      fputqqs(notice->z_opcode, stdout);
      fputs(") (sender . ", stdout);
      fputqs(notice->z_sender, stdout);
      fputs(") (recipient . ", stdout);
      if (notice->z_recipient[0])
	 fputqs(notice->z_recipient, stdout);
      else
	 fputs("nil", stdout);
      fprintf(stdout,
	      ") (port . %d) (auth . %s) (time . \"%s\") (fromhost . ",
	      notice->z_port, auth_string(auth), 
	      time_str(notice->z_time.tv_sec));
      fputqs(from_host, stdout);
      fputs(") (message . (", stdout);
#endif
}

void
process_reply(ZNotice_t *notice, int auth)
{
   PendingReply **link;
   PendingReply *reply = NULL;

   /* remove from the pending reply list */
   link = &globals->pending_replies;
   while (*link != NULL) {
      if (ZCompareUID(&notice->z_uid, &(*link)->uid)) {
	 reply = *link;
	 *link = (*link)->next;
	 break;
      }
      link = &(*link)->next;
   }

   if (NULL == reply)
      /* who called us, anyway ? */
      return;

   /* ignore replies to heartbeat zgrams */
   if (!strcmp(notice->z_class, TZC_HEARTBEAT_CLASS) &&
       !strcmp(notice->z_class_inst, TZC_HEARTBEAT_INSTANCE)) {
      /* it's a reply to a heartbeat zgram--ignore it */
      if (globals->debug) {
         printf(";;; %s received %s for heartbeat zgram\n",
		debug_time_str(time(0)),
		strcmp(notice->z_message, ZSRVACK_SENT) ? "NACK" : "ACK");
	 fflush(stdout);
      }
   } else {
      /* genuine reply, deal with it */
      switch (notice->z_kind) {
       case SERVACK:
	 if (!strcmp(notice->z_message, ZSRVACK_SENT)) {
	    /* tell emacs message sent successfully */
	    emacs_put_open();
	    emacs_put_sym("tzcspew", "sent");
	    emacs_put_str_str("to", reply->instance, reply->recipient);
	    emacs_put_close();
	 } else if (!strcmp(notice->z_message, ZSRVACK_NOTSENT)) {
	    emacs_put_open();
	    emacs_put_sym("tzcspew", "not-sent");
	    emacs_put_str_str("to", reply->instance, reply->recipient);
	    emacs_put_close();
	 } else {
	    /* oh nooooo! */
	    emacs_put_open();
	    emacs_put_sym("tzcspew", "error");
	    emacs_put_str("message", "bad reply code");
	    emacs_put_int("code", notice->z_kind);
	    emacs_put_close();
	 }
	 break;

       case SERVNAK:
	 /* tell emacs authorization failure */
	 emacs_error("authorization failure");
	 break;

       default:
	 /* server failure when receiving acknowledgement */
	 emacs_error("server failure when processing acknowledgement");
	 break;
      }
   }

   free(reply->instance);
   free(reply->recipient);
   free(reply);
}

void
read_zephyr()
{
   ZNotice_t notice;
   struct sockaddr_in from;
   int auth;
   int expected_reply;
   PendingReply *reply;

   while (ZPending() > 0) {
      expected_reply = 0;

      warn(ZReceiveNotice(&notice, &from), "ZReceiveNotice");
      auth = ZCheckAuthentication(&notice, &from);

      if (notice.z_kind != ACKED) {
	 /* check to see whether this is an ack/nak or whether it is new
	  * incoming info */
	 reply = globals->pending_replies;
	 while (reply != NULL) {
	    if (ZCompareUID(&notice.z_uid, &reply->uid)) {
	       expected_reply = 1;
	       break;
	    }
	    reply = reply->next;
	 }
      }

      if (expected_reply)
	 process_reply(&notice, auth);
      else
	 report_zgram(&notice, auth);

      (void) ZFreeNotice(&notice);
   }
}

void
send_heartbeat_zgram()
{
   /* send a zgram on a special class/instance to see if the zephyr
    * server is still talking to us. */

   char *recipient, *instance;
   
   instance = malloc(sizeof(TZC_HEARTBEAT_INSTANCE));
   strcpy(instance, TZC_HEARTBEAT_INSTANCE);

   /* broadcast the heartbeat */
   recipient = strcpy(malloc(sizeof("")),"");

   if (send_zgram_to_one(TZC_HEARTBEAT_CLASS, "", NULL,
			 instance, recipient,
                         TZC_HEARTBEAT_MESSAGE,
			 sizeof(TZC_HEARTBEAT_MESSAGE), ZNOAUTH) != 0) {
      /* failed, but we don't care. */
   }
}


void check_heartbeat() {
   if (time(0) >= globals->heartbeat.wakeup) {
      if (globals->heartbeat.status == HB_TESTING) {
	 /* Looks like the server has forgotten about us. */
	 /* We could automatically resubscribe here, but we'll just
	  * let emacs decide what to do. */

	 /* Send the warning message */
	 emacs_put_open();
	 emacs_put_sym("tzcspew", "cutoff");
	 emacs_put_close();
	 /* reset flags */
	 reset_heartbeat();
      } else if (globals->heartbeat.status == HB_ENABLED) {
	 send_heartbeat_zgram();
	 globals->heartbeat.status = HB_TESTING;
	 globals->heartbeat.wakeup = time(0) + globals->heartbeat.timeout;
	 if (globals->debug) {
		 printf(";;; %s ",debug_time_str(time(0)));
		 printf("sent heartbeat zgram, will timeout at %s\n",
			debug_time_str(globals->heartbeat.wakeup));
	 }
      }
   }
}

void reset_alarm(long alarm_rate) {
   if (alarm_rate > 0) {
      struct itimerval value;
      value.it_interval.tv_usec = 0;
      value.it_interval.tv_sec = alarm_rate;
      value.it_value.tv_usec = 0;
      value.it_value.tv_sec = alarm_rate;
      setitimer(ITIMER_REAL, &value, (struct itimerval *)0);
   }
}

int main(int argc, char *argv[]) {
   const char *program;
   int use_zctl = 0, sw;
   extern char *optarg;
   long restart_rate = 0;	/* In seconds; zero disables auto restart */

   globals = &global_storage;
   program = strrchr(argv[0], '/');
   if (program == NULL)
      program = argv[0];
   else
      program++;
   globals->program = program;
   globals->argc = argc;
   globals->argv = argv;
   globals->pidfile = NULL;
   globals->exposure = NULL;
   globals->hostname = NULL;
   globals->use_stdin = 1;
   globals->ignore_eof = 0;
   globals->heartbeat.status = HB_ENABLED;
   globals->heartbeat.period = HEARTBEAT_PERIOD_DEF;
   globals->heartbeat.timeout = HEARTBEAT_TIMEOUT;

   globals->debug = 0;

   srandom(time(0)+getpid());

   globals->location = malloc(strlen("tzc.")+16+1);
   if (globals->location == NULL) {
     fprintf(stderr,"tzc.c: out of memory\n");
     bail(1);
   }
   sprintf(globals->location,"tzc.%d",(int) getpid());

   say_hi();

   signal(SIGUSR1, restart_tzc);
   signal(SIGALRM, restart_tzc);
   signal(SIGINT, kill_tzc);
   signal(SIGHUP, kill_tzc);
   signal(SIGTERM, kill_tzc);
   signal(SIGQUIT, kill_tzc);

   while ((sw = getopt(argc, argv, "sa:e:p:l:noidt:h:")) != EOF)
      switch (sw) {
       case 's':
	 use_zctl = 1;
	 /* XXX we can't guarantee that the user has subscribed to
	  *     the heartbeat class/inst, so disable it to be safe.
          *     Could check it with ZGetSubscriptions(). */
	 globals->heartbeat.status = HB_DISABLED;
	 break;
       case 'e':
	 if (globals->exposure) {
	   free(globals->exposure);
	 }
	 globals->exposure = strdup(optarg);
	 break;
       case 'a':
	 restart_rate = atoi(optarg);
	 break;
       case 't':
	 globals->heartbeat.period = atoi(optarg);
	 printf("; heartbeat period = %d\n", (int)globals->heartbeat.period);
	 if (globals->heartbeat.period == 0)
	    globals->heartbeat.status = HB_DISABLED;
	 break;
       case 'd':
	 globals->debug = 1;
	 break;
       case 'p':
	 if (1) {
	    FILE *f = fopen(optarg, "w");
	    globals->pidfile = optarg;
	    if (f) {
	       fprintf(f, "%d\n", (int) getpid());
	       fclose(f);
	    } else {
	       perror(optarg);
	       exit(1);
	    }
	 }
	 break;
       case 'l':
	 free(globals->location);
	 globals->location = strdup(optarg);
	 break;
       case 'h':
	 globals->hostname = strdup(optarg);
       case 'o':
	 globals->use_stdin = 0;
	 break;
       case 'i':
	 globals->ignore_eof = 1;
	 break;
       case '?':
       default:
	 usage();
	 bail(1);
      }

   setup(use_zctl);

   set_location();

   reset_alarm(restart_rate);

   emacs_put_open();
   emacs_put_sym("tzcspew", "start");
   emacs_put_str("version", TZC_VERSION);
   emacs_put_int("pid",getpid());
   emacs_put_str("zephyrid", ZGetSender());
   emacs_put_str("exposure", globals->exposure==NULL?"NONE":globals->exposure);
   emacs_put_sym("heartbeat", globals->heartbeat.status == HB_DISABLED ?
		 "nil" : "t");
   emacs_put_int("heartbeat_rate", globals->heartbeat.period);
   emacs_put_str("time" , time_str(time(0)));
   emacs_put_pair_open("features");
   putc('(', stdout);
#if QUERY_HELL
   fputqqs("queries", stdout);	putc(' ', stdout);
#endif   
   putc(')', stdout);   
   emacs_put_pair_close();

   emacs_put_close();

   while (1) {
      int emacs_in, zephyr_in;

      await_input(&emacs_in, &zephyr_in);

      /* if emacs input ready, add it to a buffer */
      if (emacs_in) {
 	 if (read_emacs() != 0) {
	    if (globals->ignore_eof) {
               emacs_in = 0;
	       globals->use_stdin = 0;
	    } else {
	       exit_tzc();	/* EOF: emacs or socket disappeared. */
	    }
	 }
      }
      if (zephyr_in) {
	 reset_alarm(restart_rate);
	 read_zephyr();
      }

      check_heartbeat();
   }
}


#include <stdio.h>

void *HellStartup();
void  HellShutdown(void *hellCookie);
void *HellCompileQuery (void *hellCookie, const char *queryp);
int   HellEvaluateQuery(void *hellCookie, void *queryCookie, const char *zgram, int length);


int main()
{
  /*  char testZgram[]="kosak\000Apt pupa\000Thu Oct 15 17:06:56 1998\000PERSONAL\000repeatable?\000908485616\0001\000\001"; */
  /* char testZgram[]="kosak\000Apt pupa\000Thu Oct 15 17:14:28 1998\000 graffiti.misc.talk\000impressive.  any exponential curves at all?  microsoft/\n\000908486068\0001\000\001"; */
  char testZgram[]="acm\000Andy Myers\000Thu Oct 15 17:18:40 1998\000graffiti.misc.talk\000i believe this talk never left text mode, nor did it ever leave the\nrealm of industrial/safety-critical, thus no microsoft mentions\neither.\000908486320\0001\000\001";

  void *hellCookie=HellStartup();

  void *q1=HellCompileQuery(hellCookie, "instance:chat");
#if 0
  void *q2=HellCompileQuery(hellCookie, "sender:kosak");
  void *q3=HellCompileQuery(hellCookie, ")))))))");
#endif

  printf("string is %ld long\n", sizeof(testZgram)-1);

#if 0
  printf("queries are %p %p %p\n", q1, q2, q3);
#else
  printf("query is %p\n", q1);
#endif

  if(q1)
    printf("q1 result is %d\n", HellEvaluateQuery(hellCookie, q1, testZgram, sizeof(testZgram)-1));
#if 0
  if(q2)
    printf("q2 result is %d\n", HellEvaluateQuery(hellCookie, q2, testZgram, sizeof(testZgram)-1));
  if(q3)
    printf("q3 result is %d\n", HellEvaluateQuery(hellCookie, q3, testZgram, sizeof(testZgram)-1));
#endif

  HellShutdown(hellCookie);

  return 0;
}

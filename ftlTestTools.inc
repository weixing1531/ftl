#if defined(__GFORTRAN__)
#define ASSERT(x) call assert(x,"x",__FILE__,__LINE__)
#elif defined(NAGFOR)
#define ASSERT(x) call assert(x,__FILE__,__LINE__)
#else
#define ASSERT(x) call assert(x,#x,__FILE__,__LINE__)
#endif

#define NONLITCOUNT 4
#define LITCOUNT 25
#define NTOKENS 29
#define EOFTOKEN 1
#if defined(__cplusplus) || defined(__STDC__)
extern void mkidn(const char *, int, int *, int *),
 mkint(const char *, int, int *, int *),
 dummy1(char *, int, int *, int *);
#else
extern void mkidn(),
 mkint(),
 dummy1();
#endif
#if defined(__cplusplus) || defined(__STDC__)
extern char *auxCComment(char *, int),
 *auxEOL(char *, int),
 *dummy2(char *, int);
#else
extern char *auxCComment(),
 *auxEOL(),
 *dummy2();
#endif
#ifdef MONITOR
int mon_nonlit_codes[] = {
 6,
 13,
 15001,
 15001,
 -1
};
CONST char *mon_token_names[] = {
 "Identifier",
 "IntNumber",
 "",
 "",
 ""
};
#endif

static short CaseTbl[256] = {
  0,  0,  0,  0,  0,  0,  0,  0, 
  0,  2,  3,  0,  0,  4,  0,  0, 
  0,  0,  0,  0,  0,  0,  0,  0, 
  0,  0,  0,  0,  0,  0,  0,  0, 
  1,  0,  0, 18,  0,  0,  0,  0, 
  0,  0,  0,  0, 19,  0, 19, 17, 
  6,  6,  6,  6,  6,  6,  6,  6, 
  6,  6, 19, 19,  0,  0,  0,  0, 
  0, 16, 15,  5,  5, 14, 13, 12, 
  5, 11,  5,  5,  5,  5,  5, 10, 
  5,  5,  9,  8,  7,  5,  5,  5, 
  5,  5,  5, 19,  0, 19,  0,  5, 
  0,  5,  5,  5,  5,  5,  5,  5, 
  5,  5,  5,  5,  5,  5,  5,  5, 
  5,  5,  5,  5,  5,  5,  5,  5, 
  5,  5,  5,  0,  0,  0,  0,  0, 
  0,  0,  0,  0,  0,  0,  0,  0, 
  0,  0,  0,  0,  0,  0,  0,  0, 
  0,  0,  0,  0,  0,  0,  0,  0, 
  0,  0,  0,  0,  0,  0,  0,  0, 
  0,  0,  0,  0,  0,  0,  0,  0, 
  0,  0,  0,  0,  0,  0,  0,  0, 
  0,  0,  0,  0,  0,  0,  0,  0, 
  0,  0,  0,  0,  0,  0,  0,  0, 
  0,  0,  0,  0,  0,  0,  0,  0, 
  0,  0,  0,  0,  0,  0,  0,  0, 
  0,  0,  0,  0,  0,  0,  0,  0, 
  0,  0,  0,  0,  0,  0,  0,  0, 
  0,  0,  0,  0,  0,  0,  0,  0, 
  0,  0,  0,  0,  0,  0,  0,  0, 
  0,  0,  0,  0,  0,  0,  0,  0, 
  0,  0,  0,  0,  0,  0,  0,  0};

static short ExtCodeTbl[256] = {
  0,  0,  0,  0,  0,  0,  0,  0, 
  0,  0,  0,  0,  0,  0,  0,  0, 
  0,  0,  0,  0,  0,  0,  0,  0, 
  0,  0,  0,  0,  0,  0,  0,  0, 
  0,  0,  0,  0,  0,  0,  0,  0, 
  0,  0,  0,  0, 16,  0, 14,  0, 
  0,  0,  0,  0,  0,  0,  0,  0, 
  0,  0, 21, 25,  0,  0,  0,  0, 
  0,  0,  0,  0,  0,  0,  0,  0, 
  0,  0,  0,  0,  0,  0,  0,  0, 
  0,  0,  0,  0,  0,  0,  0,  0, 
  0,  0,  0, 11,  0, 12,  0,  0, 
  0,  0,  0,  0,  0,  0,  0,  0, 
  0,  0,  0,  0,  0,  0,  0,  0, 
  0,  0,  0,  0,  0,  0,  0,  0, 
  0,  0,  0,  0,  0,  0,  0,  0, 
  0,  0,  0,  0,  0,  0,  0,  0, 
  0,  0,  0,  0,  0,  0,  0,  0, 
  0,  0,  0,  0,  0,  0,  0,  0, 
  0,  0,  0,  0,  0,  0,  0,  0, 
  0,  0,  0,  0,  0,  0,  0,  0, 
  0,  0,  0,  0,  0,  0,  0,  0, 
  0,  0,  0,  0,  0,  0,  0,  0, 
  0,  0,  0,  0,  0,  0,  0,  0, 
  0,  0,  0,  0,  0,  0,  0,  0, 
  0,  0,  0,  0,  0,  0,  0,  0, 
  0,  0,  0,  0,  0,  0,  0,  0, 
  0,  0,  0,  0,  0,  0,  0,  0, 
  0,  0,  0,  0,  0,  0,  0,  0, 
  0,  0,  0,  0,  0,  0,  0,  0, 
  0,  0,  0,  0,  0,  0,  0,  0, 
  0,  0,  0,  0,  0,  0,  0,  0};

static unsigned char ScanTbl[] = {
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x02, 0x04, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x01, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0xF8, 0xF8, 0xF8, 0xF8,
0xF8, 0xF8, 0xF8, 0xF8,
0xF8, 0xF8, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0xE8, 0xE8, 0xE8,
0xE8, 0xE8, 0xE8, 0xE8,
0xE8, 0xE8, 0xE8, 0xE8,
0xE8, 0xE8, 0xE8, 0xE8,
0xE8, 0xE8, 0xC8, 0xE8,
0xE8, 0x68, 0xE8, 0xE8,
0xE8, 0xA8, 0xE8, 0x00,
0x00, 0x00, 0x00, 0xE8,
0x00, 0xE8, 0xE8, 0xE8,
0xE8, 0xE8, 0xE8, 0xE8,
0xE8, 0xE8, 0xE8, 0xE8,
0xE8, 0xE8, 0xE8, 0xE8,
0xE8, 0xE8, 0xE8, 0xE8,
0xE8, 0xE8, 0xE8, 0xE8,
0xE8, 0xE8, 0xE8, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0xFF, 0xFF, 0xFF, 0xFF,
0xFF, 0xFF, 0xFF, 0xFF,
0xFF, 0xFF, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0xAB, 0xFF, 0xFF,
0xFF, 0xEF, 0xFF, 0x7F,
0xFF, 0xFF, 0xFF, 0xFF,
0xFF, 0xFF, 0xFD, 0xFB,
0xFE, 0xFF, 0xDE, 0xFF,
0xDF, 0xDF, 0xF7, 0xFF,
0xF7, 0xFF, 0xFF, 0x00,
0x00, 0x00, 0x00, 0xFF,
0x00, 0xFF, 0xFF, 0xFF,
0xFF, 0xFF, 0xFF, 0xFF,
0xFF, 0xFF, 0xFF, 0xFF,
0xFF, 0xFF, 0xFF, 0xFF,
0xFF, 0xFF, 0xFF, 0xFF,
0xFF, 0xFF, 0xFF, 0xFF,
0xFF, 0xFF, 0xFF, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0xFF, 0xFF, 0xFF, 0xFF,
0xFF, 0xFF, 0xFF, 0xFF,
0xFF, 0xFF, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0xFF, 0xFF, 0xFF,
0xFF, 0xFE, 0xFF, 0xFF,
0xFF, 0x7F, 0xFF, 0xFF,
0xFF, 0xBF, 0xFF, 0xDF,
0xF7, 0xFF, 0xFF, 0xFB,
0xFD, 0xFF, 0xFF, 0xFF,
0xFF, 0xFF, 0xEF, 0x00,
0x00, 0x00, 0x00, 0xFF,
0x00, 0xFF, 0xFF, 0xFF,
0xFF, 0xFF, 0xFF, 0xFF,
0xFF, 0xFF, 0xFF, 0xFF,
0xFF, 0xFF, 0xFF, 0xFF,
0xFF, 0xFF, 0xFF, 0xFF,
0xFF, 0xFF, 0xFF, 0xFF,
0xFF, 0xFF, 0xFF, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0xFF, 0xFF, 0xFF, 0xFF,
0xFF, 0xFF, 0xFF, 0xFF,
0xFF, 0xFF, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0xFF, 0xBF, 0xFE,
0xF7, 0xFF, 0xEF, 0xFF,
0xDF, 0xFF, 0xFF, 0xFB,
0xFD, 0xFF, 0xFF, 0xFF,
0xFF, 0xFF, 0xFF, 0xFF,
0xFF, 0xFF, 0xFF, 0xFF,
0xFF, 0xFF, 0xFF, 0x00,
0x00, 0x00, 0x00, 0x7F,
0x00, 0xFF, 0xFF, 0xFF,
0xFF, 0xFF, 0xFF, 0xFF,
0xFF, 0xFF, 0xFF, 0xFF,
0xFF, 0xFF, 0xFF, 0xFF,
0xFF, 0xFF, 0xFF, 0xFF,
0xFF, 0xFF, 0xFF, 0xFF,
0xFF, 0xFF, 0xFF, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00 };

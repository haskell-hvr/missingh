#ifndef _SHA1LIB_H_
#define _SHA1LIB_H_

#ifndef  i386   /* For ALPHA  (SAK) */
#define LITTLE_ENDIAN 
typedef          long int int64;
typedef unsigned long int uint64;
typedef          int int32;
typedef unsigned int uint32;
#else  /*i386*/
#define LITTLE_ENDIAN 
typedef          long long int int64;
typedef unsigned long long int uint64;
typedef          long int int32;
typedef unsigned long int uint32;
#endif /*i386*/

typedef struct {
  uint32 state[5];
  uint32 count[2];
  unsigned char buffer[64];
} SHA1_CTX;

void SHA1Init(SHA1_CTX* context);
void SHA1Update(SHA1_CTX* context, unsigned char* data, uint32 len);	/* JHB */
void SHA1Final(unsigned char digest[20], SHA1_CTX* context);

#endif

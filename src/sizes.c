#include <stdio.h>
#include <stddef.h>
#include "SDL/SDL.h"

#define STRINGIZE(arg)  STRINGIZE1(arg)
#define STRINGIZE1(arg) STRINGIZE2(arg)
#define STRINGIZE2(arg) #arg

#define CONCATENATE(arg1, arg2)   CONCATENATE1(arg1, arg2)
#define CONCATENATE1(arg1, arg2)  CONCATENATE2(arg1, arg2)
#define CONCATENATE2(arg1, arg2)  arg1##arg2

/* PRN_STRUCT_OFFSETS will print offset of each of the fields 
 within structure passed as the first argument.
 */
#define PRN_STRUCT_OFFSETS_1(structure, field, ...) printf(STRINGIZE(structure)":"STRINGIZE(field)" = %d\n", offsetof(structure, field));
#define PRN_STRUCT_OFFSETS_2(structure, field, ...)\
  printf(STRINGIZE(structure)":"STRINGIZE(field)" = %d\n", offsetof(structure, field));\
  PRN_STRUCT_OFFSETS_1(structure, __VA_ARGS__)
#define PRN_STRUCT_OFFSETS_3(structure, field, ...)\
  printf(STRINGIZE(structure)":"STRINGIZE(field)" = %d\n", offsetof(structure, field));\
  PRN_STRUCT_OFFSETS_2(structure, __VA_ARGS__)
#define PRN_STRUCT_OFFSETS_4(structure, field, ...)\
  printf(STRINGIZE(structure)":"STRINGIZE(field)" = %d\n", offsetof(structure, field));\
  PRN_STRUCT_OFFSETS_3(structure, __VA_ARGS__)
#define PRN_STRUCT_OFFSETS_5(structure, field, ...)\
  printf(STRINGIZE(structure)":"STRINGIZE(field)" = %d\n", offsetof(structure, field));\
 PRN_STRUCT_OFFSETS_4(structure, __VA_ARGS__)
#define PRN_STRUCT_OFFSETS_6(structure, field, ...)\
  printf(STRINGIZE(structure)":"STRINGIZE(field)" = %d\n", offsetof(structure, field));\
  PRN_STRUCT_OFFSETS_5(structure, __VA_ARGS__)
#define PRN_STRUCT_OFFSETS_7(structure, field, ...)\
  printf(STRINGIZE(structure)":"STRINGIZE(field)" = %d\n", offsetof(structure, field));\
  PRN_STRUCT_OFFSETS_6(structure, __VA_ARGS__)
#define PRN_STRUCT_OFFSETS_8(structure, field, ...)\
  printf(STRINGIZE(structure)":"STRINGIZE(field)" = %d\n", offsetof(structure, field));\
  PRN_STRUCT_OFFSETS_7(structure, __VA_ARGS__)
#define PRN_STRUCT_OFFSETS_9(structure, field, ...)\
  printf(STRINGIZE(structure)":"STRINGIZE(field)" = %d\n", offsetof(structure, field));\
  PRN_STRUCT_OFFSETS_8(structure, __VA_ARGS__)
#define PRN_STRUCT_OFFSETS_10(structure, field, ...)\
  printf(STRINGIZE(structure)":"STRINGIZE(field)" = %d\n", offsetof(structure, field));\
  PRN_STRUCT_OFFSETS_9(structure, __VA_ARGS__)

#define PRN_STRUCT_OFFSETS_11(structure, field, ...)\
  printf(STRINGIZE(structure)":"STRINGIZE(field)" = %d\n", offsetof(structure, field));\
  PRN_STRUCT_OFFSETS_10(structure, __VA_ARGS__)
#define PRN_STRUCT_OFFSETS_12(structure, field, ...)\
  printf(STRINGIZE(structure)":"STRINGIZE(field)" = %d\n", offsetof(structure, field));\
  PRN_STRUCT_OFFSETS_11(structure, __VA_ARGS__)
#define PRN_STRUCT_OFFSETS_13(structure, field, ...)\
  printf(STRINGIZE(structure)":"STRINGIZE(field)" = %d\n", offsetof(structure, field));\
  PRN_STRUCT_OFFSETS_12(structure, __VA_ARGS__)
#define PRN_STRUCT_OFFSETS_14(structure, field, ...)\
  printf(STRINGIZE(structure)":"STRINGIZE(field)" = %d\n", offsetof(structure, field));\
  PRN_STRUCT_OFFSETS_13(structure, __VA_ARGS__)
#define PRN_STRUCT_OFFSETS_15(structure, field, ...)\
  printf(STRINGIZE(structure)":"STRINGIZE(field)" = %d\n", offsetof(structure, field));\
  PRN_STRUCT_OFFSETS_14(structure, __VA_ARGS__)
#define PRN_STRUCT_OFFSETS_16(structure, field, ...)\
  printf(STRINGIZE(structure)":"STRINGIZE(field)" = %d\n", offsetof(structure, field));\
  PRN_STRUCT_OFFSETS_15(structure, __VA_ARGS__)


#define PRN_STRUCT_OFFSETS_NARG(...) PRN_STRUCT_OFFSETS_NARG_(__VA_ARGS__, PRN_STRUCT_OFFSETS_RSEQ_N())
#define PRN_STRUCT_OFFSETS_NARG_(...) PRN_STRUCT_OFFSETS_ARG_N(__VA_ARGS__) 
#define PRN_STRUCT_OFFSETS_ARG_N(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, N, ...) N 
#define PRN_STRUCT_OFFSETS_RSEQ_N() 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0

#define PRN_STRUCT_OFFSETS_(N, structure, field, ...) CONCATENATE(PRN_STRUCT_OFFSETS_, N)(structure, field, __VA_ARGS__)

#define PRN_STRUCT_OFFSETS(structure, field, ...) PRN_STRUCT_OFFSETS_(PRN_STRUCT_OFFSETS_NARG(field, __VA_ARGS__), structure, field, __VA_ARGS__)

int main(void)
{
  printf("Size of int is %lu\n", sizeof(int));
  printf("Size of float is %lu\n", sizeof(float));
  printf("Size of unsigned int is %d\n", sizeof(unsigned int));
  printf("Size of int pointer is %d\n", sizeof(int*));

  printf("Size of SDL_Surface is %d\n", sizeof(SDL_Surface));
  printf("Size of SDLKey is %d\n", sizeof(SDLKey));
  PRN_STRUCT_OFFSETS(SDL_Surface, flags, format, w, h, pitch, pixels, userdata, locked, lock_data, clip_rect, map, format_version, refcount);

  PRN_STRUCT_OFFSETS(SDL_Rect, x, y, w, h);
  PRN_STRUCT_OFFSETS(SDL_Color, r, g, b, unused);
  PRN_STRUCT_OFFSETS(SDL_Point, x, y);
  PRN_STRUCT_OFFSETS(SDL_ActiveEvent, type, gain, state);
  PRN_STRUCT_OFFSETS(SDL_keysym, scancode, sym, mod, unicode);

  printf("-----------------------\n");
  PRN_STRUCT_OFFSETS(SDL_WindowEvent, type, windowID, event, padding1, padding2, padding3, data1, data2);
  printf("Size of SDL_WindowEvent is %d\n", sizeof(SDL_WindowEvent));

  printf("-----------------------\n");
  PRN_STRUCT_OFFSETS(SDL_KeyboardEvent, type, windowID, which, state, padding1, padding2, keysym);
  printf("Size of SDL_KeyboardEvent is %d\n", sizeof(SDL_KeyboardEvent));
  
  printf("-----------------------\n");
  PRN_STRUCT_OFFSETS(SDL_TextEditingEvent, type, windowID, which, text, start, length);
  printf("Size of SDL_TextEditingEvent is %d\n", sizeof(SDL_TextEditingEvent));

  return 0;
}


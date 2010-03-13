#include <iostream>
#include <stdlib.h>
#include <SDL/SDL.h>

int main(int argc, char* argv[]){ //Our main program
  SDL_Surface *screen;
  SDL_Event event; //Events
  SDL_Rect rect;
  int done = false; //Not done before we've started...

  rect.x = 0;
  rect.y = 0;
  rect.w = 100;
  rect.h = 100;

  if(SDL_Init(SDL_INIT_VIDEO) < 0){ //Could we start SDL_VIDEO?
    std::cerr << "Couldn't init SDL"; //Nope, output to stderr and quit
    exit(1);
  }

  atexit(SDL_Quit); //Now that we're enabled, make sure we cleanup

  screen = SDL_SetVideoMode(1024, 768, 16, SDL_SWSURFACE);
  int bpp = screen->format->BytesPerPixel;
  int pitch = screen->pitch;
  printf("bpp: %d\n", bpp);
  printf("pitch: %d\n", pitch);

  if (screen->format->palette == NULL) {
    printf("NULL {ALETTE !!!!!\n");
  } else {
    printf("NOT NULL {ALETTE !!!!!\n");
  }

  Uint32 color = SDL_MapRGB(screen->format, 0xff, 0x00, 0x00);
  printf("red is: %d\n", color);
  printf("format->Rloss : %d\n", screen->format->Rloss );
  printf("format->Gloss : %d\n", screen->format->Gloss );
  printf("format->Bloss : %d\n", screen->format->Bloss );
  printf("format->Rshift : %d\n", screen->format->Rshift);
  printf("format->Gshift : %d\n", screen->format->Gshift);
  printf("format->Bshift : %d\n", screen->format->Bshift );
  printf("format->Amask: %d\n", screen->format->Amask);
  if(!screen){ //Couldn't create window?
    std::cerr << "Couldn't create screen"; //Output to stderr and quit
    exit(1);
  }

  while(!done) { //While program isn't done
    while(SDL_PollEvent(&event)) { //Poll events
      switch(event.type) { //Check event type
        case SDL_QUIT: //User hit the X (or equivelent)
          done = true; //Make the loop end
          break; //We handled the event
        case SDL_VIDEORESIZE: //User resized window
            screen = SDL_SetVideoMode(event.resize.w, event.resize.h, 32,
              SDL_HWSURFACE | SDL_RESIZABLE); // Create new window
          break; //Event handled, fetch next :)
        case SDL_KEYDOWN:
          printf("scancode: %d\n\n", event.key.keysym.scancode);
          break; //Event handled, fetch next :)
      }
      SDL_DrawPoint(screen, 600, 600, color);
      //SDL_FillRect(screen, &rect, color);
      int x = 600;
      int y = 600;
      *(Uint32 *)((Uint8 *)screen->pixels + (y) * 4096 + (x) * 4) = (Uint32) color;
      SDL_Flip(screen);
    }
  } //Program done, exited
}


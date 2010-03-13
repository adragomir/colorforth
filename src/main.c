#include "SDL.h"
#include <unistd.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

extern void xcf ();
SDL_Surface *surface;

void swap () {
    SDL_Flip (surface);
}

void sdl () {
    SDL_Init (SDL_INIT_VIDEO);
    surface = SDL_SetVideoMode (1024, 768, 16, SDL_SWSURFACE);
    if (surface == NULL) exit (0);
    SDL_EnableKeyRepeat (125, 50);
    SDL_WM_SetCaption ("colorForth", "colorForth");
}

int queue[64], q = 0, p = 0;

void poll () {
    SDL_Event event;
    if (p >= 64 ) p = 0;
    while (SDL_PollEvent (&event) == 1){
	if (event.type == SDL_KEYDOWN) {
	    if (p >= 64 ) p = 0;
	    if (event.key.keysym.scancode == 113) queue[p++] = 40;
	    else if (event.key.keysym.scancode >= 24 &&
		     event.key.keysym.scancode <= 65)
		queue[p++] = event.key.keysym.scancode-24;
	}}}

int key () {
    poll ();
    if (q >= 64) q = 0;
    if (q == p) return 4;
    return queue[q++];
}

int main () {
    int fd = open ("color.com", O_RDONLY);
    struct stat s; fstat (fd, &s);
    read (fd, &xcf, s.st_size); close (fd);
    sdl (); xcf (surface->pixels, swap, key);
    return 0;
}

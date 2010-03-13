#include <GL/gl.h>
#include <GL/glx.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

extern void xcf ();
Display *display;
Window window;

static void error (void *b, char *m) {
    if (!b) { printf ("Error %s\n", m); exit(1); }}

static void glx () { XSizeHints hints;
    int attrib[] = {GLX_RGBA,GLX_RED_SIZE, 1,
		    GLX_GREEN_SIZE, 1, GLX_BLUE_SIZE, 1,
		    GLX_DOUBLEBUFFER, None };
    int screen = DefaultScreen (display);
    XSetWindowAttributes attr;
    Window root = RootWindow (display, screen);
    GLXContext context;
    XVisualInfo *info = glXChooseVisual (display, screen, attrib);
    error (info, "couldn't get an RGB, Double-buffered visual");
    attr.background_pixel = 0;
    attr.border_pixel = 0;
    attr.colormap = XCreateColormap (display, root, info->visual, AllocNone);
    attr.event_mask = KeyPressMask | StructureNotifyMask | ExposureMask;
    window = XCreateWindow
	(display, root, 0, 0, 1024, 768, 0, info->depth, InputOutput,
	 info->visual, CWBackPixel | CWBorderPixel
	 | CWColormap | CWEventMask, &attr );
    context = glXCreateContext (display, info, NULL, True);
    error (context, "glXCreateContext failed");
    glXMakeCurrent (display, window, context);
    XMapWindow (display, window);
    hints.width  = 1024;
    hints.height = 768;
    hints.flags = USSize;
    XSetNormalHints(display, window, &hints);
    XSetStandardProperties(display, window, "colorForth", "colorForth",
			   None, (char **)NULL, 0, &hints);
}



static void resize (int width, int height) {
   glViewport (0, 0, width, height);
   glMatrixMode (GL_PROJECTION);
   glLoadIdentity ();
   glOrtho( 0, width, height-0.25, -0.25, -1, 1);
   glMatrixMode (GL_MODELVIEW);
   glLoadIdentity ();    
}

int key () {
    XEvent event;
    if (XPending(display) > 0) {
	XNextEvent (display, &event);
	switch (event.type) {
	case KeyPress:
	    if (event.xkey.keycode == 113) return 40;
	    else if (event.xkey.keycode >= 24 &&
		     event.xkey.keycode <= 65)
		return event.xkey.keycode-24;
	    break;
	case ConfigureNotify:
	    resize (event.xconfigure.width, event.xconfigure.height);
	    break;
	}
    }
    return 4;
}

void color (int c) { glColor3ub (c>>16,(c>>8)&0xff,c&0xff); }
void swap () { glXSwapBuffers (display, window); }
void emit (int x, int y, int c) {
    glRasterPos2i (x, y+24);
    glBitmap (16,24,0,0,0,0,(unsigned char *)xcf+(12*1024)+((24*16/8)*c));
}
void line (int x, int y, int n) {
    glBegin (GL_LINES);
    glVertex2i (x, y); glVertex2i (x+n, y);
    glEnd ();
}
int main () {
    int fd = open ("color.com", O_RDONLY);
    struct stat s; fstat (fd, &s);
    read (fd, &xcf, s.st_size); close (fd);
    display = XOpenDisplay(NULL); glx (); resize (1024, 768);
    glPixelStorei (GL_UNPACK_ALIGNMENT, 1);
    xcf (color, swap, emit, emit, line, glRecti, key, 0);
}

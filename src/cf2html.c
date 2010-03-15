/* Convert colorForth to html */
#include <stdio.h>

char ch[] = {' ', 'r', 't', 'o', 'e', 'a', 'n', 'i',
  's', 'm', 'c', 'y', 'l', 'g', 'f', 'w',
  'd', 'v', 'p', 'b', 'h', 'x', 'u', 'q',
  '0', '1', '2', '3', '4', '5', '6', '7',
  '8', '9', 'j', '-', 'k', '.', 'z', '/',
  ';', ':', '!', '+', '@', '*', ',', '?'};

void print_text (unsigned int t) {
  while (t) {
    if (!(t & 0x80000000)) {
      putchar (ch[t >> 28]);
      t <<= 4;
    } else if ((t & 0xc0000000) == 0x80000000) {
      putchar (ch[8 + ((t >> 27) & 7)]);
      t <<= 5;
    } else {
      putchar (ch[((t >> 28) - 10) * 8 + ((t >> 25) & 7)]);
      t <<= 7;
    }
  }
}

char *function[] = {"", "execute", "execute", "define", "compile",
  "compile", "compile", "compilemacro", "execute",
  "text", "textcapitalized", "textallcaps",
  "variable", "", "", "", "", "", "executehex", "",
  "", "compilehex", "compilehex", "", "executehex"};

void print_tags (int p, int t) {
  if (p) {
    printf ("</code>");
  }
  if (t == 3 && p)
    printf ("<br>");
  printf ("<code class=%s>", function[t]);
  if (t != 3)
    putchar (' ');
}

char hex[] = {'0', '1', '2', '3', '4', '5', '6', '7',
  '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'};

void print_hex (unsigned int i) {
  int n = 8, f = 0;
  if (i == 0) {
    putchar ('0'); return;
  }
  while (n--) {
    if (!(i & 0xf0000000)) {
      if (f)
        putchar ('0');
    } else {
      f = 1;
      putchar (hex [i >> 28]);
    }
    i <<= 4;
  }
}

void print_dec (int i) {
  int j, k, f = 0;
  if (i == 0) {
    putchar ('0'); return;
  }
  if (i < 0) {
    putchar ('-'); i = -i;
  }
  for (j = 1000000000; j != 0; j /= 10) {
    k = i / j;
    if (k == 0) {
      if (f)
        putchar ('0');
    } else {
      i -= j * k;
      f = 1;
      putchar (hex[k]);
    }
  }
}

int main () {
  int b = 0, w, p = 0, t, n, pos = 0;
  pos = 0;
  printf ("<html>\n");
  printf ("<link rel=stylesheet type=\"text/css\" href=\"colorforth.css\">\n");
  if (fread (&t, 4, 1, stdin) == 0) return 0;
  pos = 4;
  while (1) {
    printf ("{block %d}\n", b++);
    printf ("<div class=code>\n");
    w = 256;
    while (w--) {
      printf("<!-- pos: %d -->", pos);
      switch (t & 0xf) {
        case 0:
          print_text (t & 0xfffffff0);
          break;
        case 2: case 5:
          print_tags (p, t & 0x1f);
          if (w == 0)
            break;
          fread (&n, 4, 1, stdin);
          pos += 4;
          w--;
          if (t & 0x10)
            print_hex (n);
          else
            print_dec (n);
          break;
        case 6: case 8:
          print_tags (p, t & 0x1f);
          if (t & 0x10)
            print_hex (t >> 5);
          else
            print_dec (t >> 5);
          break;
        case 0xc:
          print_tags (p, t & 0xf);
          print_text (t & 0xfffffff0);
          if (w == 0)
            break;
          fread (&t, 4, 1, stdin); 
          pos += 4;
          w--;
          print_tags (1, 4);
          print_dec (t);
          break;
        default:
          print_tags (p, t & 0xf);
          print_text (t & 0xfffffff0);
          break;
      }
      p = 1;
      if (fread (&t, 4, 1, stdin) == 0) {
        printf ("</code>\n</div>\n");
        goto end;
      }
      pos += 4;
    }
    if (p) {
      printf ("</code>\n");
    }
    p = 0;
    printf ("</div>\n<hr>\n");
  }
end:
  printf ("</html>\n");
  return 0;
}

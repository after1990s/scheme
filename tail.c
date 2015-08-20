#include <stdio.h>

int inc(int i)
{
  printf("%d\n",i);
  i = i + 1;
  return inc (i);
};
int main(void)
{
  return inc (0);
}

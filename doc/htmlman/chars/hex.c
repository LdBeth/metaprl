int main()
{
   char buffer[10];
   int x, i;

   while(gets(buffer)) {
      sscanf(buffer, "%x", &x);
      for(i = 7; i >= 0; i--) {
         if(x & (1 << i))
            printf("%c%c%c", 0, 0, 0);
         else
            printf("%c%c%c", 255, 255, 255);
      }
   }
}

function to_hex (s  , n, k) {
   n = substr(s, 1, 1);
   k = substr(s, 2, 1);
   n = HEX[n] * 16 + HEX[k];
   return sprintf("%c", n);
}

BEGIN { for (i = 0; i <= 15; i++) { HEX[sprintf("%x", i)] = i; } }

{
   for (i = 1; i <= NF; i++) { printf to_hex($i); }
}

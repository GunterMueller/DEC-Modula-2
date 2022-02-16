# define ENDOFFILE 0
# define PLUS 1
# define MINUS 2
# define ASTERISK 3
# define SLASH 4
# define ASSIGN 5
# define AMPERSAND 6
# define DOT 7
# define COMMA 8
# define SEMICOLON 9
# define LPAREN 10
# define LBRACKET 11
# define LBRACE 12
# define UPARROW 13
# define EQUALS 14
# define SHARP 15
# define LESS 16
# define GREATER 17
# define NOTEQUAL 18
# define LSEQUAL 19
# define GREQUAL 20
# define DOTDOT 21
# define COLON 22
# define RPAREN 23
# define RBRACKET 24
# define RBRACE 25
# define BAR 26
# define IDENT 27
# define NUMBER 28
# define UNUSED 29
# define CHARCONST 30
# define STRCONST 31
# define BOOLCONST 32
# define AND 33
# define ARRAY 34
# define BEGIN 35
# define BY 36
# define CASE 37
# define CONST 38
# define LABEL 39
# define DIV 40
# define DO 41
# define ELSE 42
# define GOTO 43
# define END 44
# define PACKED 45
# define FORWARD 46
# define FOR 47
# define FROM 48
# define IF 49
# define FUNCTION 50
# define EXTERNAL 51
# define IN 52
# define DOWNTO 53
# define MOD 54
# define PROGRAM 55
# define NOT 56
# define OF 57
# define OR 58
# define POINTER 59
# define PROCEDURE 60
# define PFILE 61
# define RECORD 62
# define REPEAT 63
# define RETURN 64
# define SET 65
# define THEN 66
# define TO 67
# define TYPE 68
# define UNTIL 69
# define VAR 70
# define WHILE 71
# define WITH 72
# define INCLUDE 73
# define BAD 74
#define yyclearin yychar = -1
#define yyerrok yyerrflag = 0
extern int yychar;
extern short yyerrflag;
#ifndef YYMAXDEPTH
#define YYMAXDEPTH 150
#endif
#ifndef YYSTYPE
#define YYSTYPE int
#endif
YYSTYPE yylval, yyval;
# define YYERRCODE 256

# line 966 "pascal.gram"


PrintEND()
{
	PrintSemi();
	PrintKeyword("END");
	PrintSemi();
}
short yyexca[] ={
-1, 1,
	0, -1,
	-2, 0,
-1, 134,
	23, 167,
	-2, 169,
-1, 174,
	21, 79,
	-2, 288,
-1, 280,
	9, 165,
	42, 165,
	44, 165,
	69, 165,
	-2, 222,
-1, 425,
	44, 120,
	-2, 126,
	};
# define YYNPROD 299
# define YYLAST 538
short yyact[]={

  83, 417, 245,  66, 405, 222, 426, 261,  68, 408,
  31,  31,  31, 366, 130,  79, 156, 247, 155, 231,
 233, 162, 339,  14, 146, 248,  14,  65,  67,  32,
  87,  74,  73,  39,  70,  27,  69, 265, 451, 384,
  30, 403,  31,  31,  31, 424,  16,  15,   6,  16,
  15,  34,  37,  26,  31,  31, 385, 150,  24, 423,
 406,  24, 330,   5, 119, 120, 244, 147,  25, 243,
  46,  25, 436, 419, 335,  45,  17, 149,  18,  17,
 240,  18, 142,  34,  37,  35,  99, 265,  28, 268,
 125, 126,  33, 418,  95,  96, 288, 387, 282, 430,
 386, 299, 143, 170, 287, 170,  31, 208, 285, 154,
 281,  29, 111, 106, 113, 115, 112, 114, 116, 219,
 129, 121, 369, 178, 284, 217,  28, 127, 264,  86,
 133, 389, 283, 286, 131,  85, 132, 239,  26,  28,
 376, 128,  80,  82, 191, 199, 179, 182, 229, 204,
 117, 183, 302,  84, 372, 394, 186, 184, 185,  28,
  75, 258,  76,  77, 390, 193, 175, 176, 192, 260,
 371, 345, 346, 201, 203, 171, 263, 257, 165, 153,
  49,  28, 205, 206, 259, 454, 228, 200,  81, 224,
 221, 108, 174, 172, 152, 173, 223,  28, 340, 227,
 342, 343, 258, 204,  52, 442, 309, 225, 241, 220,
 177, 239, 204, 204,  71,  72, 429, 253, 239, 255,
 175, 176, 230,  80,  82, 301, 167, 169, 237, 171,
 168,  63, 238, 263, 317, 280,  92, 308, 249, 307,
  28,  75, 306,  76,  77,  62, 174, 172, 252, 173,
 279, 251, 250, 109, 388,  60, 133,  58, 138, 232,
 131, 290, 132, 313,  13, 295, 318,  13, 133,  81,
 312, 456, 131, 133, 132, 450, 134, 131, 134, 132,
  51,  50,  47,  50,  56, 449, 300, 289, 266, 265,
 197, 196, 170, 329, 195, 319, 170, 170, 170, 139,
 107, 103, 101,  97, 344, 344, 344,  94, 327,  91,
  90,  57, 332, 328, 334, 333,  38, 239, 108, 353,
 354, 355, 293, 356, 105, 365, 365, 347, 348, 360,
 361, 362,  89,   4, 292, 368,  98, 373, 204, 170,
 364, 367, 377, 363, 341, 123, 122, 180, 181, 232,
 344, 344, 344, 344, 118, 374, 124, 375, 194, 137,
 314, 262, 382, 311, 310, 110, 188, 187, 383, 136,
 135, 170, 378, 379, 380, 381, 357, 358,  78, 326,
 401, 400, 325, 432, 232, 431, 439, 422, 399, 391,
 324, 398, 323, 170, 397, 322, 438, 409, 407, 396,
 321, 365, 410, 437, 453, 416, 414, 411, 412, 402,
 443, 435, 428, 395, 413, 421, 367, 420, 223, 320,
 226, 190, 189, 359, 277, 276, 275, 274, 273, 272,
 271, 434, 433, 270, 269, 267, 234, 278, 316, 315,
 440, 441, 444, 445, 446, 291, 148, 202, 236, 297,
 210, 204, 448, 452, 447, 427, 404, 457, 455, 425,
 415, 393, 336, 338, 337, 246, 298, 211, 296, 209,
 392, 331, 370, 294, 242, 166, 207, 352, 256, 350,
 254, 351, 218, 349, 216, 305, 215, 304, 214, 303,
 213, 212, 164, 163, 161, 160, 159, 158, 157, 145,
  93,  44,  19, 235, 144,  48,  20,  53,  22,  21,
  54,  23, 104,  36,  61, 141,  43, 102,  59, 140,
  42, 100,  41,  40,  64,  12,  11,  10,   9,   8,
   7, 198,  88, 151,  55,   3,   2,   1 };
short yypact[]={

   8,-1000,-1000,-1000,  11,  99,-1000,-1000,-1000,  99,
  99,  99,-1000, 307, -40,-1000,-1000,-1000,-1000,  24,
-1000,-1000, 273, 271,-1000,-1000,-1000, 274,-1000, 302,
 243,-1000,-1000,-1000, 241,-1000, 223,-1000,-1000,-1000,
 213,  99,  99,  99,  11, 301, 300,-1000, 214,-1000,
-1000,-1000, 298,  99,  99, 294,  99,-1000,-1000, 293,
-1000, 292,-1000,-1000,  82, 291, 310,-1000, 232,  98,
  63,-1000,-1000,  87,-1000,-1000,-1000,-1000,-1000, 266,
-1000,-1000,-1000, 246, 290,-1000,-1000,  38,  67,  11,
-1000,-1000,-1000,   7,-1000,-1000,-1000,-1000, 171,-1000,
 213,-1000, 165,-1000, 165,  99,-1000,-1000,-1000,-1000,
 213,-1000,-1000,-1000,-1000,-1000,-1000,-1000, 132,-1000,
-1000,-1000, 132, 132, 132,-1000,-1000,-1000,-1000,-1000,
-1000,-1000,-1000,-1000,-1000, 213, 132, 213,-1000,-1000,
 285, 282, 281,-1000,  99, 164,-1000,-1000,  99,-1000,
-1000,  11,-1000,  99,-1000,-1000,-1000,-1000,-1000,-1000,
-1000,-1000,-1000,-1000,-1000,-1000,  73,-1000,-1000,-1000,
-1000,-1000,-1000,-1000,-1000,  97,  91,-1000,-1000,-1000,
 213, 213,-1000,  87,  63,  63,-1000, 213,  99, 184,
 213, 163,-1000, 124, 213,-1000,-1000,-1000,-236,-1000,
-1000,-1000,  99, 210,-1000,  36,-1000,  99,-1000,  12,
   9,  99,  99, 231, 230, 227,  99,-1000,  99,-1000,
-1000,-1000, 153,-1000,-1000,-1000, 161, 211,-1000,-1000,
 103, 280,-1000, 279,  61, 278,   7, 309,-1000,-1000,
 315,-1000, 254,-1000,-1000,  64, 277,-1000, 203, 129,
-1000,-1000,-1000, 221, 218, 216, 185,-1000,-1000,-1000,
-1000,-1000, 154,-1000,-1000,-1000,-1000,-1000, 212,-1000,
-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000, 261,
 268,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-236,-1000,
-1000,  99,  99,-1000,   5, 219, 165, 219,  30,-1000,
-1000,-1000,-1000, 170, 170, 170,-1000,-1000,-1000,-1000,
 213, 213,-1000, 211, 213,-1000,-1000,-1000,-1000,-1000,
 213, 213, 213,-236,  99,  99, 213,  78,-1000,-1000,
-1000, 146,-1000,-1000,-1000,-1000,  99,  99, 165,-1000,
-1000, 112,-1000,-1000,-1000,-1000,-1000,-1000,-1000, 170,
 170, 170, 170,-1000, 211,-1000,-1000,-1000,-1000, 213,
 -27,  -1,  59,  28, 249,-1000,-1000, 123,-1000,-1000,
 165,-1000,-1000, 133,-1000,-1000,-1000,-1000,-1000,-1000,
-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,
-1000,-1000, 219, -16,-1000,-1000,-1000,-1000, 213, 213,
  99,-1000,-1000,-1000,  99,  51,-1000,  29,-1000, 213,
-1000,-1000,  -8,-1000,-1000,-1000,-1000,-1000,-1000,-1000,
-1000, 194,  58,-1000,-1000,-1000,-1000, 213,  23,-1000,
-1000, 213, 213,-1000, 183,-1000,-1000,-1000,-1000,-1000,
-1000,-1000,-1000, 213,-1000, 276,-1000, 265, -28,-1000,
  99,-1000, 162,-1000, 262,  51,-1000,-1000 };
short yypgo[]={

   0, 537, 536, 535, 332,  29, 534, 533,  30, 532,
 531,  19,  48, 530, 529, 528, 527, 526, 525, 524,
 523,  27, 522, 111,  40, 521,   8, 520,  92, 519,
 518, 517,  18, 516,  85, 515, 514, 513, 512, 511,
 510, 509, 180, 508, 507, 506, 505, 504,   0, 503,
 502, 501, 500, 499,  16, 498, 497, 496, 495, 494,
  21, 493, 492, 491,  25, 490, 489,  22, 488, 487,
 486, 485, 484, 483, 482, 481, 480, 479, 478, 477,
 476, 475, 474, 473, 472, 471, 470, 469, 468, 467,
   2, 466, 465,  17, 464, 463, 462, 461, 460, 459,
 456,   6, 455,   3, 454, 450, 449,  24, 448, 447,
 446, 445,  60, 439, 438,   4, 437, 436, 435, 434,
 433, 430, 429, 428, 427, 426, 425, 424,  15, 423,
  14, 422, 421, 420, 419, 413,   1, 412, 411, 410,
 404, 403, 400, 399, 398,   9, 397,   5, 396, 395,
 394, 392, 391, 390, 388, 387, 386, 385, 383, 382,
  13, 381, 380, 379,  31, 378, 370, 369, 367, 366,
  36, 365, 364,   7, 363, 361, 360, 359, 358, 356,
 354,  28, 348, 347,  32,  34, 346, 345, 344, 336,
 334, 324 };
short yyr1[]={

   0,   1,   2,   2,   7,   3,  10,   8,   9,   9,
   4,   4,  12,  12,  12,  12,  12,  12,  12,  19,
  13,  20,  14,  22,  15,  15,  25,  23,  27,  29,
  16,  30,  16,  31,  28,  33,  35,  17,  36,  17,
  38,  34,  40,  39,  41,  41,  44,  43,  45,  47,
  49,  45,  50,  50,  51,  18,  18,  18,  46,  46,
  52,  42,  32,  32,  32,  32,  32,  32,  54,  54,
  54,  63,  61,  65,  66,  62,  68,  69,  62,  70,
  71,  62,  72,  73,  62,  74,  75,  62,  76,  77,
  62,  78,  79,  62,  80,  55,  82,  84,  56,  83,
  85,  86,  85,  81,  81,  87,  88,  57,  89,  59,
  90,  92,  94,  92,  93,  95,  93,  91,  96,  98,
  91,  97, 100,  97,  99,  99, 102, 104, 101, 105,
 106,  58,  53, 108,  53, 109, 107, 111, 107, 110,
 110, 110,  11, 113,  11, 114,  11, 115, 116, 112,
 117, 117, 118, 118, 118, 118, 118, 118, 118, 118,
 118, 118, 118, 129, 119, 120, 120, 131, 130, 132,
 130, 134, 135, 121, 136, 137, 136, 139, 140, 138,
 141, 138, 142, 143, 122, 144, 144, 146, 148, 145,
 149, 150, 123, 151, 152, 124, 153, 154, 156, 125,
 157, 155, 158, 155, 159, 126, 161, 160, 162, 160,
 163, 127, 164, 164, 164, 164, 164, 164, 166, 164,
 167, 164, 128, 168, 128, 128, 169, 128,  26,  26,
 147, 172, 147, 133, 174, 133, 173, 173, 173, 176,
 175, 177, 165, 178, 165,  21,  21, 179, 179, 179,
 179, 179, 180, 180, 180, 171, 171, 171, 171, 171,
 171, 171, 181, 182, 181, 103, 183, 103, 184, 184,
 170, 186, 170, 187, 170, 185, 185, 188, 188,  60,
  48,   6,   6,  64, 190,  64, 189, 189,   5,  67,
  67,  67,  67,  67,  67,  24,  37, 191,  37 };
short yyr2[]={

   0,   1,   1,   1,   0,   8,   0,   4,   0,   1,
   1,   2,   1,   1,   1,   1,   1,   1,   2,   0,
   4,   0,   4,   0,   4,   3,   0,   4,   0,   0,
   5,   0,   4,   0,   4,   0,   0,   5,   0,   4,
   0,   4,   0,   3,   2,   3,   0,   3,   2,   0,
   0,   7,   1,   1,   0,   5,   3,   3,   0,   1,
   0,   4,   1,   1,   1,   1,   1,   1,   1,   1,
   1,   0,   4,   0,   0,   5,   0,   0,   5,   0,
   0,   5,   0,   0,   6,   0,   0,   6,   0,   0,
   6,   0,   0,   6,   0,   3,   0,   0,   7,   3,
   1,   0,   4,   0,   1,   0,   0,   5,   0,   5,
   1,   1,   0,   4,   0,   0,   4,   0,   0,   0,
   7,   0,   0,   3,   1,   2,   0,   0,   8,   0,
   0,   5,   1,   0,   4,   0,   3,   0,   5,   0,
   1,   1,   1,   0,   4,   0,   4,   1,   3,   2,
   0,   3,   0,   1,   1,   1,   1,   1,   1,   1,
   1,   1,   1,   0,   4,   1,   2,   0,   3,   0,
   4,   0,   0,   7,   0,   0,   3,   0,   0,   7,
   0,   2,   0,   0,   7,   1,   2,   0,   0,   6,
   0,   0,   6,   0,   0,   6,   0,   0,   0,  10,
   0,   3,   0,   3,   0,   3,   0,   4,   0,   4,
   0,   3,   1,   1,   1,   1,   1,   2,   0,   4,
   0,   3,   1,   0,   5,   2,   0,   4,   1,   3,
   1,   0,   4,   2,   0,   5,   0,   2,   3,   0,
   3,   0,   4,   0,   5,   0,   1,   1,   1,   1,
   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,
   1,   1,   1,   0,   4,   1,   0,   4,   1,   3,
   1,   0,   3,   0,   3,   1,   3,   1,   1,   1,
   1,   0,   3,   1,   0,   4,   1,   3,   1,   1,
   2,   1,   1,   2,   1,   1,   1,   0,   4 };
short yychk[]={

-1000,  -1,  -2,  -3,  -4,  55, -12, -13, -14, -15,
 -16, -17, -18, 256,  15,  39,  38,  68,  70, -50,
 -45, -41, -43, -39,  50,  60, -12,  -5,  27, -23,
 -24, -48,  -5, -28, -24, -34, -37, -24,   9,  73,
 -20, -22, -27, -33, -51,  51,  46,   9, -46, -42,
  10,   9, -42, -44, -40,  -6,  10,   9,  14, -30,
  14, -36,  22,   8, -19, -21,-103,-181, -26,-170,
-185,   1,   2,-184,-164,  28,  30,  31,-165,-128,
  10,  56,  11, -48, -23, -28, -34,  -8,  -9,  -4,
   9,   9,  22, -52,   9, -24, -24,   9,-189,  -5,
 -25,   9, -31,   9, -38,-191,  31,   9,   8,  21,
-171,  14,  18,  16,  19,  17,  20,  52,-180,   1,
   2,  58,-186,-187,-179,   3,   4,  40,  54,  33,
-130,  11,  13,   7,  10,-166,-167,-177,  12,   9,
 -29, -35,  44,  35, -47, -53,-107,  60,-110,  70,
  50,  -7,  23,   8, -26, -32, -54, -55, -56, -57,
 -58, -59, -60, -61, -62,  13, -81,  61,  65,  62,
 -48,  10,  28,  30,  27,   1,   2,  45, -32, -24,
-183,-182,-170,-184,-185,-185,-164,-168,-169,-131,
-132, -26,-164, -21,-178,   9,   9,   9, -10, -48,
  23,   9,-109, -64, -48,  -8,  -5, -80,  34, -87,
-105, -89, -63, -65, -68, -70, -72,  28, -74,  28,
-181, -26,-147, -26, -48,  23,-133, -26,  23,  24,
 -21, -11,-112, 256,-117, -49,-108, -64,  22,   8,
  44, -48, -82,  57,  57, -90, -92, -93, -64, -64,
  21,  21,  21, -48, -76, -48, -78,  24,   8,  23,
   8,-173,-175,  22,  25,   9,   9,-118,  28,-119,
-120,-121,-122,-123,-124,-125,-126,-127,-116,-128,
 -48,  49,  37,  71,  63,  47,  72,  43,  35,   9,
-107,-111,-190,   7, -83,  11, -88,-106, -91,  37,
   9,  22,  23, -66, -69, -71,  21,  21,  21,  21,
-172,-174,-173, -48,-176,-113,-114,  22,   5,-130,
-134,-142,-149,-151,-153,-159,-163, -11, -60, -48,
  57, -85, -54, -32, -54,  44, -96, -94, -95, -67,
  28,-188,  30,  31, -48,   1,   2, -67, -67, -73,
 -77, -75, -79, -26, -26,-173, -26,-112,-112,-129,
 -26, -26, -26, -11,-128, -48,-160,-128, -26,  44,
 -84,  24,   8, -48, -93, -32,  28, -48, -67, -67,
 -67, -67,-173, -26,  66,  57,  41,  69,   5,   8,
  41, -32, -86, -97,  22,-135,-143,-150,-152,-154,
-161,-162, -54,  57,-100,-115,-112,-144,-145,-146,
-115, -26, -26,-160,-115, -98, -48,-136,  42,  44,
-145,-147,-155,  67,  53, -99,-101,-102,-137,  22,
  41,-157,-158,-101,-103,-138,  49,-141,-148,-156,
 -26, -26,  22,-139,-115,-115,-115,-104, -26,   9,
  10,  66, -90,-140,  23,-115,   9,-136 };
short yydef[]={

   0,  -2,   1,   2,   3,   0,  10,  12,  13,  14,
  15,  16,  17,   0,   0,  21,  23,  28,  35,  54,
  52,  53,  58,   0,  46,  42,  11, 281, 288,   0,
   0, 295, 280,  31,   0,  38,   0, 296,  18,  19,
 245,   0,   0,   0,   8,   0,   0,  48,   0,  59,
  60,  44,   0,   0,   0,   0,   0,  25,  26,   0,
  33,   0,  40, 297,   0,   0, 246, 265, 262, 228,
 270, 271, 273, 275, 268, 212, 213, 214, 215, 216,
 218, 220, 241, 222,   0,  29,  36,   0,   0,   9,
  56,  57,  49, 139,  45,  47,  43,   4,   0, 286,
   0,  32, 103,  39, 103,   0,  20,  22, 266, 263,
   0, 255, 256, 257, 258, 259, 260, 261,   0, 252,
 253, 254,   0,   0,   0, 247, 248, 249, 250, 251,
 217, 223, 225, 226,  -2,   0,   0, 245, 243,  24,
   0,   0,   0,   6,   0,   0, 132, 135,   0, 140,
 141,   8, 282,   0,  27,  34,  62,  63,  64,  65,
  66,  67,  68,  69,  70,  94,   0, 105, 129, 108,
 279,  71,  73,  76,  -2,  82,  85, 104,  41, 298,
   0,   0, 229, 276, 272, 274, 269,   0,   0,   0,
   0,   0, 221,   0, 245,  30,  37,  55, 150,  50,
  61, 133,   0,   0, 283,   0, 287,   0,  96,   0,
   0, 114,   0,   0,   0,   0,   0,  88,   0,  91,
 267, 264,   0, 230, 227, 168,   0, 236, 219, 242,
   0,   7, 142,   0, 152,   0, 139, 136, 137, 284,
   0,  95,   0, 106, 130, 117, 110, 111,   0,   0,
  74,  77,  80,   0,   0,   0,   0, 224, 231, 170,
 234, 233, 236, 239, 244, 143, 145, 149,   0, 153,
 154, 155, 156, 157, 158, 159, 160, 161, 162,   0,
  -2, 171, 182, 190, 193, 196, 204, 210, 150,  51,
 134,   0,   0,   5,   0,   0, 103,   0,   0, 118,
 112, 115,  72,   0,   0,   0,  83,  89,  86,  92,
   0,   0, 237, 236,   0, 150, 150, 151, 163, 166,
   0,   0,   0, 150,   0,   0,   0,   0, 138, 285,
  97,   0, 100, 107, 131, 109,   0, 114, 103,  75,
 289,   0, 291, 292, 294, 277, 278,  78,  81,   0,
   0,   0,   0, 232, 236, 238, 240, 144, 146,   0,
   0,   0,   0,   0,   0, 222, 205,   0, 211, 148,
 103,  99, 101, 121, 113, 116, 290, 293,  84,  90,
  87,  93, 235, 164, 172, 183, 191, 194, 197, 206,
 208,  98,   0,   0, 122, 150, 187, 150,   0,   0,
   0, 150, 102, 119,   0, 174, 147, 187, 185,   0,
 192, 195,   0, 207, 209, 126, 123, 173, 175, 184,
 186,   0,   0, 200, 202,  -2, 124,   0, 180, 188,
 198,   0,   0, 125,   0, 176, 177, 150, 150, 150,
 201, 203, 127,   0, 181,   0, 199,   0,   0, 189,
 114, 178,   0, 150,   0, 174, 128, 179 };
#ifndef lint
static char yaccpar_sccsid[] = "@(#)yaccpar	4.1	(Berkeley)	2/11/83";
#endif not lint

#
# define YYFLAG -1000
# define YYERROR goto yyerrlab
# define YYACCEPT return(0)
# define YYABORT return(1)

/*	parser for yacc output	*/

#ifdef YYDEBUG
int yydebug = 0; /* 1 for debugging */
#endif
YYSTYPE yyv[YYMAXDEPTH]; /* where the values are stored */
int yychar = -1; /* current input token number */
int yynerrs = 0;  /* number of errors */
short yyerrflag = 0;  /* error recovery flag */

yyparse() {

	short yys[YYMAXDEPTH];
	short yyj, yym;
	register YYSTYPE *yypvt;
	register short yystate, *yyps, yyn;
	register YYSTYPE *yypv;
	register short *yyxi;

	yystate = 0;
	yychar = -1;
	yynerrs = 0;
	yyerrflag = 0;
	yyps= &yys[-1];
	yypv= &yyv[-1];

 yystack:    /* put a state and value onto the stack */

#ifdef YYDEBUG
	if( yydebug  ) printf( "state %d, char 0%o\n", yystate, yychar );
#endif
		if( ++yyps> &yys[YYMAXDEPTH] ) { yyerror( "yacc stack overflow" ); return(1); }
		*yyps = yystate;
		++yypv;
		*yypv = yyval;

 yynewstate:

	yyn = yypact[yystate];

	if( yyn<= YYFLAG ) goto yydefault; /* simple state */

	if( yychar<0 ) if( (yychar=yylex())<0 ) yychar=0;
	if( (yyn += yychar)<0 || yyn >= YYLAST ) goto yydefault;

	if( yychk[ yyn=yyact[ yyn ] ] == yychar ){ /* valid shift */
		yychar = -1;
		yyval = yylval;
		yystate = yyn;
		if( yyerrflag > 0 ) --yyerrflag;
		goto yystack;
		}

 yydefault:
	/* default state action */

	if( (yyn=yydef[yystate]) == -2 ) {
		if( yychar<0 ) if( (yychar=yylex())<0 ) yychar = 0;
		/* look through exception table */

		for( yyxi=yyexca; (*yyxi!= (-1)) || (yyxi[1]!=yystate) ; yyxi += 2 ) ; /* VOID */

		while( *(yyxi+=2) >= 0 ){
			if( *yyxi == yychar ) break;
			}
		if( (yyn = yyxi[1]) < 0 ) return(0);   /* accept */
		}

	if( yyn == 0 ){ /* error */
		/* error ... attempt to resume parsing */

		switch( yyerrflag ){

		case 0:   /* brand new error */

			yyerror( "syntax error" );
		yyerrlab:
			++yynerrs;

		case 1:
		case 2: /* incompletely recovered error ... try again */

			yyerrflag = 3;

			/* find a state where "error" is a legal shift action */

			while ( yyps >= yys ) {
			   yyn = yypact[*yyps] + YYERRCODE;
			   if( yyn>= 0 && yyn < YYLAST && yychk[yyact[yyn]] == YYERRCODE ){
			      yystate = yyact[yyn];  /* simulate a shift of "error" */
			      goto yystack;
			      }
			   yyn = yypact[*yyps];

			   /* the current yyps has no shift onn "error", pop stack */

#ifdef YYDEBUG
			   if( yydebug ) printf( "error recovery pops state %d, uncovers %d\n", *yyps, yyps[-1] );
#endif
			   --yyps;
			   --yypv;
			   }

			/* there is no state on the stack with an error shift ... abort */

	yyabort:
			return(1);


		case 3:  /* no shift yet; clobber input char */

#ifdef YYDEBUG
			if( yydebug ) printf( "error recovery discards char %d\n", yychar );
#endif

			if( yychar == 0 ) goto yyabort; /* don't discard EOF, quit */
			yychar = -1;
			goto yynewstate;   /* try again in the same state */

			}

		}

	/* reduction by production yyn */

#ifdef YYDEBUG
		if( yydebug ) printf("reduce %d\n",yyn);
#endif
		yyps -= yyr2[yyn];
		yypvt = yypv;
		yypv -= yyr2[yyn];
		yyval = yypv[1];
		yym=yyn;
			/* consult goto table to find next state */
		yyn = yyr1[yyn];
		yyj = yypgo[yyn] + *yyps + 1;
		if( yyj>=YYLAST || yychk[ yystate = yyact[yyj] ] != -yyn ) yystate = yyact[yypgo[yyn]];
		switch(yym){
			
case 4:
# line 156 "pascal.gram"
{ SetProgFile(); } break;
case 6:
# line 162 "pascal.gram"
{ PrintKeyword("BEGIN"); } break;
case 18:
# line 190 "pascal.gram"
{ SourceError("declaration error"); } break;
case 19:
# line 194 "pascal.gram"
{ PrintString("(* #"); PrintKeyword("INCLUDE");} break;
case 20:
# line 196 "pascal.gram"
{ PrintStringConst(yypvt[-0]); PrintString(" *)");
			ProcessInclude(yypvt[-0]); } break;
case 21:
# line 200 "pascal.gram"
{ PrintKeyword("LABEL"); } break;
case 22:
# line 202 "pascal.gram"
{ PrintSemi(); SourceError("no gotos or labels"); } break;
case 23:
# line 206 "pascal.gram"
{ PrintKeyword("CONST"); } break;
case 24:
# line 208 "pascal.gram"
{ PrintSemi(); } break;
case 25:
# line 210 "pascal.gram"
{ PrintSemi(); } break;
case 26:
# line 214 "pascal.gram"
{ PrintString("="); } break;
case 28:
# line 219 "pascal.gram"
{ PrintKeyword("TYPE"); } break;
case 29:
# line 220 "pascal.gram"
{ PrintSemi(); } break;
case 31:
# line 222 "pascal.gram"
{ PrintSemi(); } break;
case 33:
# line 226 "pascal.gram"
{ PrintString("="); } break;
case 35:
# line 230 "pascal.gram"
{ PrintKeyword("VAR"); } break;
case 36:
# line 230 "pascal.gram"
{ PrintSemi(); } break;
case 38:
# line 232 "pascal.gram"
{ PrintSemi(); } break;
case 40:
# line 236 "pascal.gram"
{ PrintString(":"); } break;
case 42:
# line 240 "pascal.gram"
{ SetOutput(0, 1); } break;
case 43:
# line 241 "pascal.gram"
{ DefineFunction(yypvt[-0]); SetOutput(0, 1); yyval = yypvt[-0]; } break;
case 44:
# line 246 "pascal.gram"
{ DefineParameters(0); PrintSemi(); yyval=yypvt[-1]; } break;
case 45:
# line 248 "pascal.gram"
{ DefineParameters(1); PrintSemi(); yyval=yypvt[-2]; } break;
case 46:
# line 252 "pascal.gram"
{ SetOutput(0, 1); } break;
case 47:
# line 253 "pascal.gram"
{ DefineFunction(yypvt[-0]); SetOutput(0, 1); yyval = yypvt[-0]; } break;
case 48:
# line 257 "pascal.gram"
{ DefineParameters(0); PrintSemi(); yyval=yypvt[-1]; } break;
case 49:
# line 259 "pascal.gram"
{ PrintString(":"); } break;
case 50:
# line 260 "pascal.gram"
{ DefineParameters(1); } break;
case 51:
# line 261 "pascal.gram"
{ PrintSemi(); yyval=yypvt[-6]; } break;
case 54:
# line 272 "pascal.gram"
{ SetOutput(1, 0); PrintFunctionHeader(); } break;
case 55:
# line 274 "pascal.gram"
{ PrintKeyword("END"); PrintIdent(yypvt[-4]);
		      EndFunction(0); PrintSemi(); } break;
case 56:
# line 278 "pascal.gram"
{ SetOutput(1, 0); PrintFunctionHeader(); EndFunction(1); } break;
case 57:
# line 281 "pascal.gram"
{ SetOutput(1, 0); EndFunction(0); } break;
case 58:
# line 286 "pascal.gram"
{ PrintString("()"); } break;
case 60:
# line 291 "pascal.gram"
{ PrintString("("); } break;
case 61:
# line 293 "pascal.gram"
{ PrintString(")"); } break;
case 71:
# line 325 "pascal.gram"
{ PrintString("("); } break;
case 72:
# line 327 "pascal.gram"
{ PrintString(")"); } break;
case 73:
# line 331 "pascal.gram"
{ PrintString("[");
			    PrintConst(yypvt[-0]); } break;
case 74:
# line 333 "pascal.gram"
{ PrintString(".."); } break;
case 75:
# line 335 "pascal.gram"
{ PrintString("]"); } break;
case 76:
# line 337 "pascal.gram"
{ PrintString("[");
			    PrintStringConst(yypvt[-0]); } break;
case 77:
# line 339 "pascal.gram"
{ PrintString(".."); } break;
case 78:
# line 341 "pascal.gram"
{ PrintString("]"); } break;
case 79:
# line 343 "pascal.gram"
{ PrintString("[");
			PrintIdent(yypvt[-0]); } break;
case 80:
# line 345 "pascal.gram"
{ PrintString(".."); } break;
case 81:
# line 347 "pascal.gram"
{ PrintString("]"); } break;
case 82:
# line 349 "pascal.gram"
{ PrintString("[+"); } break;
case 83:
# line 351 "pascal.gram"
{ PrintString(".."); } break;
case 84:
# line 353 "pascal.gram"
{ PrintString("]"); } break;
case 85:
# line 355 "pascal.gram"
{ PrintString("[-"); } break;
case 86:
# line 357 "pascal.gram"
{ PrintString(".."); } break;
case 87:
# line 359 "pascal.gram"
{ PrintString("]"); } break;
case 88:
# line 362 "pascal.gram"
{ PrintString("[+");
		PrintConst(yypvt[-0]); } break;
case 89:
# line 364 "pascal.gram"
{ PrintString(".."); } break;
case 90:
# line 366 "pascal.gram"
{ PrintString("]"); } break;
case 91:
# line 369 "pascal.gram"
{ PrintString("[-");
		PrintConst(yypvt[-0]); } break;
case 92:
# line 371 "pascal.gram"
{ PrintString(".."); } break;
case 93:
# line 373 "pascal.gram"
{ PrintString("]"); } break;
case 94:
# line 377 "pascal.gram"
{PrintKeyword("POINTER");
		         PrintAtPascal();
			 PrintKeyword("TO"); EnsureSpace(); } break;
case 96:
# line 385 "pascal.gram"
{ PrintKeyword("ARRAY"); EnsureSpace(); } break;
case 97:
# line 387 "pascal.gram"
{ PrintKeyword("OF"); } break;
case 101:
# line 401 "pascal.gram"
{ PrintString(","); } break;
case 105:
# line 411 "pascal.gram"
{ PrintKeyword("FILE"); } break;
case 106:
# line 412 "pascal.gram"
{ PrintKeyword("OF"); } break;
case 107:
# line 414 "pascal.gram"
{ SourceError("use io module for files"); } break;
case 108:
# line 420 "pascal.gram"
{ PrintKeyword("RECORD"); } break;
case 109:
# line 424 "pascal.gram"
{ PrintEND(); } break;
case 112:
# line 435 "pascal.gram"
{ PrintSemi(); } break;
case 115:
# line 443 "pascal.gram"
{ PrintString(":"); } break;
case 118:
# line 450 "pascal.gram"
{ PrintKeyword("CASE"); } break;
case 119:
# line 452 "pascal.gram"
{ PrintKeyword("OF"); AdvanceSpace(); } break;
case 120:
# line 454 "pascal.gram"
{ PrintEND(); } break;
case 122:
# line 459 "pascal.gram"
{ PrintString(":");} break;
case 126:
# line 469 "pascal.gram"
{ PrintString("| "); } break;
case 127:
# line 471 "pascal.gram"
{ PrintString(":"); } break;
case 128:
# line 475 "pascal.gram"
{ PrintSemi(); } break;
case 129:
# line 479 "pascal.gram"
{ PrintKeyword("SET"); } break;
case 130:
# line 480 "pascal.gram"
{ PrintKeyword("OF"); } break;
case 133:
# line 492 "pascal.gram"
{ PrintSemi(); } break;
case 135:
# line 497 "pascal.gram"
{ PrintKeyword("PROCEDURE"); } break;
case 136:
# line 499 "pascal.gram"
{ SourceError("use procedure type for procedure parameters"); } break;
case 137:
# line 503 "pascal.gram"
{ PrintString(":"); } break;
case 140:
# line 510 "pascal.gram"
{ PrintKeyword("VAR"); } break;
case 141:
# line 512 "pascal.gram"
{ PrintKeyword("FUNCTION");
		SourceError("use procedure type for function parameters"); } break;
case 143:
# line 524 "pascal.gram"
{PrintSemi();} break;
case 145:
# line 527 "pascal.gram"
{ SourceError("statement error"); } break;
case 148:
# line 539 "pascal.gram"
{ EatSpace(); } break;
case 151:
# line 550 "pascal.gram"
{ PrintConst(yypvt[-1]); PrintString(":");
		SourceError("no gotos or labels"); } break;
case 163:
# line 581 "pascal.gram"
{ PrintString(":="); CheckFunction(yypvt[-1]); } break;
case 167:
# line 592 "pascal.gram"
{ PrintString("("); } break;
case 168:
# line 593 "pascal.gram"
{ PrintString(")"); } break;
case 169:
# line 595 "pascal.gram"
{ PrintString("("); } break;
case 170:
# line 597 "pascal.gram"
{ PrintString(")"); } break;
case 171:
# line 602 "pascal.gram"
{ PrintKeyword("IF");} break;
case 172:
# line 603 "pascal.gram"
{ PrintKeyword("THEN"); EatSpace();} break;
case 173:
# line 605 "pascal.gram"
{ PrintEND(); } break;
case 175:
# line 610 "pascal.gram"
{ EatSpace(); } break;
case 177:
# line 614 "pascal.gram"
{ PrintKeyword("ELSIF"); } break;
case 178:
# line 615 "pascal.gram"
{ PrintKeyword("THEN"); EatSpace(); } break;
case 180:
# line 618 "pascal.gram"
{ PrintKeyword("ELSE"); } break;
case 182:
# line 623 "pascal.gram"
{ PrintKeyword("CASE"); } break;
case 183:
# line 625 "pascal.gram"
{ PrintKeyword("OF"); AdvanceSpace(); } break;
case 184:
# line 628 "pascal.gram"
{ PrintEND(); } break;
case 187:
# line 638 "pascal.gram"
{ PrintString("| "); } break;
case 188:
# line 640 "pascal.gram"
{ PrintString(":"); EatSpace(); } break;
case 190:
# line 647 "pascal.gram"
{ PrintKeyword("WHILE"); } break;
case 191:
# line 649 "pascal.gram"
{ PrintKeyword("DO"); EatSpace(); } break;
case 192:
# line 651 "pascal.gram"
{ PrintEND(); } break;
case 193:
# line 655 "pascal.gram"
{ PrintKeyword("REPEAT"); } break;
case 194:
# line 657 "pascal.gram"
{ PrintKeyword("UNTIL"); } break;
case 196:
# line 663 "pascal.gram"
{ PrintKeyword("FOR"); } break;
case 197:
# line 665 "pascal.gram"
{ PrintString(":="); } break;
case 198:
# line 668 "pascal.gram"
{ PrintKeyword("DO"); EatSpace(); } break;
case 199:
# line 670 "pascal.gram"
{ PrintEND(); } break;
case 200:
# line 674 "pascal.gram"
{ PrintKeyword("TO"); } break;
case 202:
# line 677 "pascal.gram"
{ PrintKeyword("TO"); } break;
case 203:
# line 679 "pascal.gram"
{ PrintKeyword("BY -1"); EnsureSpace(); } break;
case 204:
# line 684 "pascal.gram"
{ PrintKeyword("WITH"); } break;
case 206:
# line 689 "pascal.gram"
{ PrintString(","); } break;
case 207:
# line 690 "pascal.gram"
{ PrintEND(); } break;
case 208:
# line 692 "pascal.gram"
{ PrintKeyword("DO"); EatSpace(); } break;
case 209:
# line 693 "pascal.gram"
{ PrintEND(); } break;
case 210:
# line 697 "pascal.gram"
{ PrintKeyword("GOTO"); } break;
case 211:
# line 699 "pascal.gram"
{ SourceError("no gotos or labels"); } break;
case 212:
# line 705 "pascal.gram"
{ PrintConst(yypvt[-0]); } break;
case 213:
# line 707 "pascal.gram"
{ PrintStringConst(yypvt[-0]); } break;
case 214:
# line 709 "pascal.gram"
{ PrintStringConst(yypvt[-0]); } break;
case 218:
# line 717 "pascal.gram"
{ PrintString("("); } break;
case 219:
# line 719 "pascal.gram"
{ PrintString(")"); } break;
case 220:
# line 721 "pascal.gram"
{ PrintKeyword("NOT"); } break;
case 223:
# line 734 "pascal.gram"
{ PrintString("["); } break;
case 224:
# line 736 "pascal.gram"
{ PrintString("]"); } break;
case 225:
# line 738 "pascal.gram"
{ PrintString("^"); } break;
case 226:
# line 740 "pascal.gram"
{ PrintString("."); } break;
case 231:
# line 755 "pascal.gram"
{ PrintString(","); } break;
case 234:
# line 765 "pascal.gram"
{ PrintString(","); } break;
case 239:
# line 778 "pascal.gram"
{ PrintString(":"); } break;
case 241:
# line 782 "pascal.gram"
{ PrintString("{"); } break;
case 242:
# line 784 "pascal.gram"
{ PrintString("}"); } break;
case 243:
# line 787 "pascal.gram"
{ PrintString("{"); } break;
case 244:
# line 789 "pascal.gram"
{ PrintString("}"); } break;
case 247:
# line 802 "pascal.gram"
{ PrintString("*"); } break;
case 248:
# line 804 "pascal.gram"
{ PrintString("/"); } break;
case 249:
# line 806 "pascal.gram"
{ PrintKeyword("DIV"); } break;
case 250:
# line 808 "pascal.gram"
{ PrintKeyword("MOD"); } break;
case 251:
# line 810 "pascal.gram"
{ PrintKeyword("AND"); } break;
case 252:
# line 816 "pascal.gram"
{ PrintString("+"); } break;
case 253:
# line 818 "pascal.gram"
{ PrintString("-"); } break;
case 254:
# line 820 "pascal.gram"
{ PrintKeyword("OR"); } break;
case 255:
# line 826 "pascal.gram"
{ PrintString("="); } break;
case 256:
# line 828 "pascal.gram"
{ PrintString("<>"); } break;
case 257:
# line 830 "pascal.gram"
{ PrintString("<"); } break;
case 258:
# line 832 "pascal.gram"
{ PrintString("<="); } break;
case 259:
# line 834 "pascal.gram"
{ PrintString(">"); } break;
case 260:
# line 836 "pascal.gram"
{ PrintString(">="); } break;
case 261:
# line 838 "pascal.gram"
{ PrintKeyword("IN"); } break;
case 263:
# line 846 "pascal.gram"
{ PrintString(".."); } break;
case 266:
# line 855 "pascal.gram"
{ PrintString(","); } break;
case 271:
# line 870 "pascal.gram"
{ PrintString("+"); } break;
case 273:
# line 873 "pascal.gram"
{ PrintString("-"); } break;
case 277:
# line 886 "pascal.gram"
{ PrintString("+"); } break;
case 278:
# line 888 "pascal.gram"
{ PrintString("-"); } break;
case 280:
# line 901 "pascal.gram"
{ PrintIdent(yypvt[-0]);} break;
case 284:
# line 913 "pascal.gram"
{ PrintString(","); } break;
case 289:
# line 933 "pascal.gram"
{ PrintConst(yypvt[-0]); } break;
case 290:
# line 936 "pascal.gram"
{ PrintConst(yypvt[-0]); } break;
case 291:
# line 938 "pascal.gram"
{ PrintStringConst(yypvt[-0]); } break;
case 292:
# line 940 "pascal.gram"
{ PrintStringConst(yypvt[-0]); } break;
case 295:
# line 954 "pascal.gram"
{ CheckExport(yypvt[-0]); } break;
case 297:
# line 961 "pascal.gram"
{ PrintString(","); } break; 
		}
		goto yystack;  /* stack new state and value */

	}

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
# define CARDCONST 28
# define REALCONST 29
# define CHARCONST 30
# define STRCONST 31
# define UNUSEDXXX1 32
# define AND 33
# define ARRAY 34
# define BEGIN 35
# define BY 36
# define CASE 37
# define CONST 38
# define DEFINITION 39
# define DIV 40
# define DO 41
# define ELSE 42
# define ELSIF 43
# define END 44
# define EXIT 45
# define EXPORT 46
# define FOR 47
# define FROM 48
# define IF 49
# define IMPLEMENTATION 50
# define IMPORT 51
# define IN 52
# define LOOP 53
# define MOD 54
# define MODULE 55
# define NOT 56
# define OF 57
# define OR 58
# define POINTER 59
# define PROCEDURE 60
# define QUALIFIED 61
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
# define ATSIZE 73
# define ATALIGN 74
# define ATPASCAL 75
# define ATC 76
# define ATNOCHECK 77
# define ATNILCHECK 78
# define ATUNUSEDxxx3 79
# define ATNOCOUNT 80
# define ATEXTERNAL 81
# define ATGLOBAL 82
# define ATASM 83
# define ATDYNARRAY 84
# define ATSUBARRAY 85
# define ATLOCAL 86
# define ATLEFTTORIGHT 87
# define ATRIGHTTOLEFT 88
# define ATUNUSEDxxx4 89
# define ATINLINE 90
# define ATSHARED 91
# define ATNOINIT 92
# define DOWNTO 93
# define TKFILE 94
# define FORWARD 95
# define FUNCTION 96
# define GOTO 97
# define LABEL 98
# define PACKED 99
# define PROGRAM 100
# define OCT 101
# define HEX 102
# define ENDOFLINE 103
# define TKSMALLIDENT 104
# define TKSMALLSTRCONST 105
# define NADA 106

# line 176 "pas.gram"
#include <stdio.h>
#define YYMAXDEPTH	500
/* standard type pointers, globally defined in symtab */
#ifdef vms
globalref int anyTypeNode;
#endif
#ifdef unix
int anyTypeNode;
#endif
int temp;

#define	EXPROCT	0	/* For creating ExprNode's */
#define EXPRHEX	1

extern Errors_Error();
yyerror(s) char *s; {
    Errors_Error((int *) s, 1023);
}

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
short yyexca[] ={
-1, 1,
	0, -1,
	-2, 0,
-1, 7,
	256, 7,
	-2, 4,
-1, 127,
	21, 214,
	-2, 191,
	};
# define YYNPROD 233
# define YYLAST 685
short yyact[]={

 100, 196, 392, 379, 137, 383, 154, 256, 120, 286,
 380, 245, 330, 255, 246, 157, 216, 204, 198,  41,
 121,  41,  50,  77, 158, 159,   5, 153, 167, 158,
 159,  32,  11, 168, 170,  47, 179,  24, 168, 170,
 144, 359,  65, 145, 112,  31, 199, 369, 102,  76,
 101, 162, 163, 164, 165, 101, 162, 163, 164, 165,
 396, 366, 113, 114, 181, 287,  78, 360, 363, 116,
  40, 127, 250, 111, 127, 161,  53, 140, 268, 169,
  37, 266, 139, 248, 169, 101,  90,  92, 101, 117,
 118, 119, 115,  91, 195,  95, 201, 251, 200,  58,
 334, 270,  15, 172, 345,  98,  43,  94,  92, 104,
 220, 105,  56,  57, 116, 116, 354, 140,  55,  42,
  92,  97, 193, 171,  20, 192, 289, 288, 176,  96,
  99, 239,  16, 272,  17, 219,  75, 322, 152, 127,
 186, 143, 234, 235, 289, 288, 141, 207, 185, 160,
 180, 355, 210, 213, 194,  93, 217,  67, 188, 189,
  21, 273,  14,  26, 209,  41, 314,  13, 277,  92,
 148, 257,  30, 211, 146, 241, 147, 247, 101, 236,
 249, 252,   4, 279, 205, 263, 339, 205, 158, 159,
 313, 214, 127, 237, 410, 267,  39, 168, 170, 156,
  41, 127, 127,  92, 250,  15,  33, 116, 240, 275,
  61, 280, 265, 202, 101, 162, 163, 164, 165, 254,
 208, 278, 276, 175, 253,  63, 106,  20, 261, 251,
 101, 117, 118, 119,  36,  16,  35,  17,  44,  38,
  62,   7, 290, 169,  64, 242, 332,  92,  54, 409,
 182,  51,  63, 285, 310, 247, 282, 312,  45, 259,
 318, 327, 331,  21,  48,  14, 328, 127, 284, 127,
 127,  64, 283, 320, 338, 127, 321, 335,  92, 155,
 324,  92, 323, 340, 372,  92, 325, 333, 395, 342,
 336, 337, 217,  38, 344, 341, 326, 329, 388, 367,
 234, 235, 343, 357, 184, 142, 180, 258, 231, 233,
 306, 307, 308, 309, 393, 158, 159, 382, 348,  70,
 315, 351, 247, 353, 168, 170, 315,  71, 394, 362,
 352, 316, 206, 127, 387,  68, 269, 236,  63, 361,
 356, 101, 162, 163, 164, 165,  63, 150, 191, 365,
 358, 237, 274, 364, 368, 305, 151,  64, 404, 403,
 260, 247, 103,  23, 127,  64, 371, 116, 385, 370,
 169, 384, 377, 109,  92, 110, 386, 378, 298, 299,
 373, 374, 391, 389, 390,  92,  92, 375, 376, 311,
  63, 315, 116, 281, 116, 262, 384, 116, 384, 402,
 405, 384, 401,  38,  73, 406, 407, 190, 408,  64,
 187, 397, 398, 399, 400, 177,  92,  92,  92,  92,
 140, 291, 292, 293, 294, 295, 296, 297, 148, 158,
 159, 149, 146,  74, 147, 113, 114,  13, 168, 170,
 150,  72, 148, 319, 136, 149, 146, 135, 147, 151,
 346, 347,  69, 349, 350, 101, 162, 163, 164, 165,
  34, 101, 117, 118, 119, 115, 108,   8, 130,  66,
 168, 170, 234, 235, 234, 235, 158, 159, 303, 304,
 300, 301, 228, 229, 169, 168, 170, 101, 162, 163,
 164, 165,  46, 243, 138,  49, 132, 128, 203, 134,
 129, 122, 101, 162, 163, 164, 165, 124, 123, 236,
 197, 236, 271, 113, 114, 221, 169, 223, 225, 222,
 224, 226, 136, 237, 381, 237, 125, 302, 133, 238,
 126, 169,  52, 131, 113, 114, 212, 232, 230, 101,
 117, 118, 119, 115, 166, 215,  80,  82,  79,  81,
  83,  84, 218, 227,  85, 317,  86, 174,  87,  88,
 101, 117, 118, 119, 115,  89,  60,  59,  19,  18,
 244,  29,  28,  27,  25,  12, 183, 178,   6, 107,
  10, 264,  22,   9,   3,   2,   1,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0, 173 };
short yypact[]={

 -74,-1000,-1000,-1000,-1000, 214,-1000,-1000, 167, 353,
-219,-1000,-1000,-1000, 135,-1000,-1000,-1000,-1000, -50,
 209, 207, 394, 173,  75, 230,-1000,   8,  -5,  -8,
  74, 394, 394,-1000,  64,-1000,-1000,-1000,-1000,-1000,
 217,-1000,-1000,-1000, 129,-1000,-1000, 321, 443,-1000,
 305, 432,-1000, 382, 424, 394,-1000,-1000,  58, 352,
 352,  67,-1000, 199,-1000,-1000, 366,-1000, 533,-1000,
 434,-1000,-1000, 434,-1000,-1000, 411,-1000,-1000,  58,
-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,
 283,  58, 435, 110,  23,  23, 428,-1000, 196, 151,
-1000,-1000, 406,  -6, 282,-1000,-1000, 437,-1000,-1000,
-1000, 401,-1000, 203, 203,-1000,-1000,-1000,-1000,-1000,
 398,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,
 337,  60, 138,  41,  39, 186, 157, 311, 434, 394,
  58,-1000,-1000, 108,-1000,  23, 475,-1000, 164, 475,
-1000,-1000,-1000,  69,-1000,-1000, 501, 481, 460, 460,
 471,  98,-1000,-1000,-1000,-1000,-1000, 421, 475, 460,
 314,  26,  31,  31,  58, 342, 163,-1000, 284,-1000,
 338, 138,-1000, 386, 151,-1000,-1000,-1000,-1000,-1000,
-1000, 512,  24, 138,  21, 325,  57, 124,-1000, 330,
 434, 512,-1000, 160,-1000,-1000, 533, 384,-1000,-1000,
-1000,-1000, 248,-1000,-1000, 245,-1000,  43,  58,-1000,
-1000, 475, 475, 475, 475, 475, 475, 475, 460, 460,
 479, 469, 477, 297, 460, 460, 460, 460, 460, 460,
-1000, 234,-1000, 166, 142, 383,-1000, 310, 187,  58,
-1000,-1000,  58,  68,  23,-1000,  58, 151,-1000,  -6,
 151, 244,-1000,-1000, 437, 238, 512,  56, 434, 512,
-1000,-1000, 138, 159, 434,-1000,-1000, 157,-1000,-1000,
-1000,-1000, 475,-1000,-1000, 475,-1000, 475,-1000,-1000,
  62,-1000,-1000,-1000,-1000,-1000,-1000,-1000, 139, 139,
 460, 460, 460, 460, 460, 460,-1000,-1000,-1000,-1000,
-1000,-1000,-1000,-1000,-1000, 475, 475, 107, 318, 281,
-1000,-1000,  23, -26,-1000, 163,-1000,-1000, 151,-1000,
-1000,  11, 512,-1000,-1000,-1000, 238,-1000,   4, 277,
-1000,-1000,-1000,-1000,  25,  58, 139, 139,-1000, 139,
 139,-1000,-1000,-1000,  28,-1000,  58,  58,-1000,  23,
  23,-1000,-1000, 434, 238,-1000,  61, 151,-1000, 475,
-1000, 312, 276,-1000,-1000,  31,  31,-1000,-1000, 373,
-1000, 306, 266,-1000,-1000,   3,-1000,  58,  58,  58,
  58,  61,-1000, 533, 349, 348,  61,-1000,-1000,-1000,
-1000,-1000,-1000, 138, 138, 373, 226, 171,-1000,-1000,
-1000 };
short yypgo[]={

   0, 586, 585, 584, 583, 582,  80, 172,  42, 581,
  32, 580, 579,  46, 578, 460,  36,   0,  48, 577,
 576, 575, 574, 573, 572, 571, 569,  49, 568, 567,
 566,   7, 565,  28,  13,  23,  43, 559,  27, 558,
 557,   6, 556, 555,  11, 554, 552, 551, 550,  40,
 549, 548, 547, 546,  66, 545,  75, 544, 149,  15,
 538, 537, 199, 536,   9,  16, 532,   8, 530, 526,
  20,   5,   4, 524,  14,  10,   1,  18,   3,   2,
 512, 510, 508, 507, 501,  12, 500,  17, 498, 497,
 495, 494, 493,  44, 492, 469, 466 };
short yyr1[]={

   0,   1,   1,   1,   4,   9,   2,  11,  12,   2,
   5,   5,   5,  14,   3,  10,  10,  16,  16,  16,
  18,  18,  20,  20,  19,  19,  21,  21,  21,  21,
  21,  15,  15,   7,  29,  28,  30,  28,  26,  26,
  26,  26,  31,  31,  32,  34,  34,  36,  36,  37,
  37,  40,  39,  38,  38,  42,  42,  43,  43,  43,
  43,  43,  43,  45,  46,  46,  47,  47,  27,  27,
  48,  48,  50,  51,  52,  53,  35,  35,  54,  54,
  54,  54,  54,  54,  54,  54,  54,  54,  54,  54,
  49,  56,  56,  56,  56,  56,  56,  56,  56,  56,
  58,  58,  58,  58,  58,  58,  59,  59,  59,  59,
  60,  60,  60,  60,  61,  61,  61,  61,  62,  62,
  62,  41,  41,  41,  41,  41,  41,  41,  41,  63,
  63,  64,  64,  65,  65,  65,  65,  65,  55,  55,
  33,  33,  33,  33,  66,  66,  25,  25,  68,  69,
  69,  71,  73,  73,  74,  74,  44,  44,  75,  75,
  75,  77,  77,  78,  78,  79,  80,  80,  76,  76,
  81,  81,  82,  82,  83,  83,  84,  84,  85,  85,
  86,  13,  13,  13,  13,  87,  88,  88,  88,  88,
  89,  70,  70,  70,  67,  67,  67,  67,  67,  67,
  90,  91,  90,  90,  24,  24,  57,  57,  92,  92,
  72,  72,  72,  72,  93,  93,  93,  93,  94,  94,
  22,  22,  22,  23,  23,  17,  95,   8,  96,  96,
  96,   6,   6 };
short yyr2[]={

   0,   1,   1,   1,   0,   0,  10,   0,   0,   8,
   0,   2,   3,   0,   3,   1,   0,   3,   4,   1,
   0,   3,   0,   2,   1,   3,   3,   2,   2,   2,
   1,   0,   2,   3,   0,   5,   0,   6,   4,   3,
   3,   2,   1,   1,   3,   2,   3,   1,   1,   8,
   8,   0,   5,   1,   1,   4,   4,   0,   5,   5,
   2,   3,   3,   5,   1,   1,   4,   6,   1,   3,
   1,   2,   3,   2,   2,   3,   0,   1,   1,   2,
   1,   1,   1,   1,   1,   1,   1,   1,   1,   1,
   3,   1,   1,   1,   1,   1,   1,   2,   3,   2,
   1,   3,   3,   3,   3,   3,   1,   3,   3,   3,
   1,   3,   3,   3,   1,   3,   3,   3,   1,   2,
   2,   1,   3,   3,   3,   3,   3,   3,   3,   1,
   3,   1,   1,   1,   2,   3,   4,   5,   1,   3,
   1,   4,   2,   3,   4,   2,   0,   2,   2,   3,
   4,   1,   1,   3,   1,   3,   1,   3,   0,   5,
   5,   0,   3,   1,   3,   0,   5,   7,   2,   1,
   1,   3,   3,   4,   3,   4,   4,   5,   3,   3,
   3,   1,   3,   2,   2,   1,   1,   3,   2,   2,
   3,   1,   1,   1,   1,   1,   1,   1,   1,   1,
   4,   0,   5,   2,   0,   2,   3,   3,   0,   1,
   1,   2,   2,   1,   1,   1,   1,   1,   4,   2,
   3,   2,   1,   0,   2,   1,   0,   2,   1,   0,
   1,   1,   0 };
short yychk[]={

-1000,  -1,  -2,  -3, 256, 100, -14,  27, -15,  -4,
 -11, -10, -21,   0,  98,  38,  68,  70, -26, -28,
  60,  96,  -5,  10, 256, -22,  28, -23, -24, -25,
  -7,  95,  81, 256, -15,  27,  27,  -6,   9,  23,
 -13,  27,  44,  -6,   8,  28, -94,  27, 256, -90,
  27, 256, -66, -13, 256,  44,  -6,  -6,  35, -29,
 -30,  -7,  23,   8,  27,  -8, -95,  28,  14,   9,
  14,  22,   9,  22,   9,  -6, -27, -35, -54, -51,
 -53, -50, -52, -48, -47, -45, -42, -39, -37, -32,
  28,  35, -33,  97,  49,  37,  71,  63,  47,  72,
 -17,  27, -18,  10, -18,  44,  27, -12, -96,   7,
   9, -72, -93,   1,   2,  31, -17,  28,  29,  30,
 -67, -70, -84, -82, -83, -69, -68, -17, -89, -86,
  34,  99,  62,  94,  65,  13,  10, -72, -91, -67,
   9, -54,  22, -27, -49, -36,  11,  13,   7,  10,
   5,  14,  28, -38, -41, 256, -62, -59,   1,   2,
 -58, -56,  28,  29,  30,  31, -57, -33,  10,  56,
  11, -38, -41, 256, -40,  27, -33,   9, -19, -16,
 -13,  70, 256, -20,  22,  -8, -10,   9, -93, -93,
   9,  11,  65,  62,  94,  34, -76, -81, -77, -13,
  57,  57,  27, -88, -87,  27,  21, -67,  -6, -35,
  44, -38, -63, -41,  27, -55, -65, -41, -46,  66,
  41,  14,  18,  16,  19,  17,  20,  52,   1,   2,
 -60, -58, -61, -58,   3,   4,  40,  54,  58,  33,
 -49, -41, -56, -92, 256, -44, -74, -41,  57, -31,
  41,  66, -31, -27, -36, -34, -31,   8,  23,  -6,
  22, -13,   9, -17,  -9, -70,  57, -76,  57,  11,
  44, -80,   9,  37,  22, -67, -70,   8, -87,  23,
 -72,   9,   8,  24,  23,   8, -64,  22, 102, 101,
 -35, -62, -62, -62, -62, -62, -62, -62, -58, -58,
   1,   2,  58,   1,   2,  58, -56, -56, -56, -56,
 -59, -58,  23,  24,  24,   8,  21, -43, -44, 256,
 -35, -35,  69, -38, -35, -33, -16, -17,  22, -10,
 -85,  24,   8, -70,  44, -67, -70, -77, -17,  27,
 -67, -87, -41, -65, -41,  42, -58, -58, -59, -58,
 -58, -59, -74, -41,   9,  44,  22,  22, -38,  67,
  93, -34, -17,  57, -70, -85,  57,  22, -64,  22,
 -35, -44, 256, -35, -35, -38, -38, -67, -85, -78,
 -75, -73, 256, -71, -72, -17, -41,  22,  22, -31,
 -31,   9, -79,   8,  22,  22,  57, -35, -35, -35,
 -35, -75, -71,  10,  10, -78, -76, -76, -79,  23,
  23 };
short yydef[]={

  13,  -2,   1,   2,   3,   0,  31,  -2,  16,  10,
   0,  14,  32,  15,   0, 223, 204, 146,  30,  31,
   0,   0, 232,   0,   0, 232, 222,  27,  28,  29,
   0, 232, 232,  41,   0,  34,  36,  31, 231,  11,
   0, 181, 226,  26,   0, 221, 224,   0,   0, 205,
   0,   0, 147,   0,   0, 232,  39,  40,  76,  20,
  20,   0,  12, 183, 184,   8, 229, 220,   0, 219,
   0, 201, 203,   0, 145,  38,  33,  68,  77,  78,
  80,  81,  82,  83,  84,  85,  86,  87,  88,  89,
   0,  76,  70,   0,   0,   0,   0,  51,   0,   0,
 140, 225,   0,   0,  22, 226, 182,  16, 227, 228,
 230,   0, 210,   0,   0, 213, 214, 215, 216, 217,
   0, 194, 195, 196, 197, 198, 199,  -2, 192, 193,
   0,   0, 161,   0,   0,   0,   0,   0,   0, 232,
  76,  79,  73,   0,  71,   0,   0, 142,   0,   0,
  47,  48,  74,   0,  53,  54, 121, 118,   0,   0,
 106, 100,  91,  92,  93,  94,  95,  96,   0,   0,
 208,   0,   0,   0,  76,   0,   0,  35, 232,  24,
   0,   0,  19,   0,   0,   5,   9, 218, 211, 212,
 200,   0,   0, 161,   0,   0,   0, 169, 170,   0,
   0,   0, 148,   0, 186, 185,   0,   0, 144,  69,
  75,  72,   0, 129, 143,   0, 138, 133,  76,  64,
  65,   0,   0,   0,   0,   0,   0,   0,   0,   0,
 119, 110, 120, 114,   0,   0,   0,   0,   0,   0,
  97,   0,  99,   0,   0, 209, 156, 154,  57,  76,
  42,  43,  76,   0,   0,  44,  76,   0,  21,   0,
   0,   0,  37,  23,  16,   0,   0,   0,   0,   0,
 172, 168, 161,   0,   0, 174, 149, 188, 189, 190,
 180, 202,   0, 141,  90,   0, 134,   0, 131, 132,
  66, 122, 123, 124, 125, 126, 127, 128, 107, 108,
   0,   0,   0,   0,   0,   0, 101, 102, 103, 104,
 109, 105,  98, 206, 207,   0,   0,   0,   0,   0,
  55,  56,   0,   0,  45,   0,  25,  17,   0,   6,
 176,   0,   0, 150, 173, 175,   0, 171,   0, 225,
 162, 187, 130, 139, 135,  76, 111, 112, 113, 115,
 116, 117, 157, 155,  60,  63,  76,  76,  52,   0,
   0,  46,  18,   0,   0, 177, 158,   0, 136,   0,
  67,   0,   0,  61,  62,   0,   0, 178, 179, 165,
 163,   0,   0, 152, 151,   0, 137,  76,  76,  76,
  76, 158, 166,   0,   0,   0, 158,  58,  59,  49,
  50, 164, 153, 161, 161, 165,   0,   0, 167, 159,
 160 };
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
			
case 3:
# line 216 "pas.gram"
{ printf("Fatal error, cannot proceed\n");
				    exit(1); } break;
case 4:
# line 221 "pas.gram"
{ yyval = DefineModule(yypvt[-0],MODULE); } break;
case 5:
# line 224 "pas.gram"
{ EndModule(yypvt[-5],yypvt[-2],yypvt[-6]); } break;
case 7:
# line 225 "pas.gram"
{ yyval = DefineModule(yypvt[-0],MODULE); } break;
case 8:
# line 226 "pas.gram"
{ EndModule(yypvt[-3],0,yypvt[-4]); } break;
case 11:
# line 233 "pas.gram"
{ yyerror("Program header cannot be empty"); } break;
case 12:
# line 235 "pas.gram"
{ ProcessFileList(yypvt[-1]); } break;
case 13:
# line 239 "pas.gram"
{ yyval = DefineModule(0,IMPLEMENTATION); } break;
case 14:
# line 240 "pas.gram"
{ EndModule(yypvt[-2],0,0); } break;
case 16:
# line 246 "pas.gram"
{ yyerror("Junk after end of program");
		  EndFile(); 
		  yychar = ENDOFFILE; } break;
case 17:
# line 253 "pas.gram"
{ temp = TypeOf(yypvt[-0]);
				  yyval = MakeParamList(0,yypvt[-2],temp); } break;
case 18:
# line 256 "pas.gram"
{ temp = TypeOf(yypvt[-0]);
				  yyval = MakeParamList(VAR,yypvt[-2],temp); } break;
case 19:
# line 259 "pas.gram"
{ yyval = MakeParamList(0,0,anyTypeNode); } break;
case 20:
# line 264 "pas.gram"
{ yyval = 0; } break;
case 21:
# line 266 "pas.gram"
{ yyval = yypvt[-1]; } break;
case 22:
# line 271 "pas.gram"
{ yyval = 0; } break;
case 23:
# line 273 "pas.gram"
{ yyval = TypeOf(yypvt[-0]); } break;
case 25:
# line 281 "pas.gram"
{ yyval = AppendParamList(yypvt[-2],yypvt[-0]); } break;
case 33:
# line 306 "pas.gram"
{ yyval = yypvt[-0]; } break;
case 34:
# line 311 "pas.gram"
{ yyval = DefineProc(yypvt[-0],PROCEDURE); } break;
case 35:
# line 313 "pas.gram"
{ temp = ProcType(yypvt[-1],0);
				    yyval = AddTypeToProc(yypvt[-2],temp); } break;
case 36:
# line 315 "pas.gram"
{ yyval = DefineProc(yypvt[-0],PROCEDURE); } break;
case 37:
# line 317 "pas.gram"
{ temp = ProcType(yypvt[-2],yypvt[-1]);
				  yyval = AddTypeToProc(yypvt[-3],temp); } break;
case 38:
# line 323 "pas.gram"
{ EndProc(yypvt[-3],yypvt[-2],0); } break;
case 39:
# line 325 "pas.gram"
{ EndProc(yypvt[-2],0,0); } break;
case 40:
# line 327 "pas.gram"
{ MakeExternal(yypvt[-2]); EndProc(yypvt[-2],0,0); } break;
case 41:
# line 329 "pas.gram"
{ EndProc(yypvt[-1],0,0); } break;
case 43:
# line 336 "pas.gram"
{ yyerror("Use DO instead of THEN"); } break;
case 44:
# line 341 "pas.gram"
{ yyval = BuildStmtWith(yypvt[-1],yypvt[-0]); } break;
case 45:
# line 346 "pas.gram"
{ yyval = yypvt[-0]; } break;
case 46:
# line 348 "pas.gram"
{ temp = BuildStmtWith(yypvt[-1],yypvt[-0]);
				  yyval = AddToStmtList(0,temp); } break;
case 48:
# line 356 "pas.gram"
{ yyerror("Use ':=' instead of '=' for assignment"); } break;
case 49:
# line 362 "pas.gram"
{ yyval = BuildStmtFor(yypvt[-6],yypvt[-4],yypvt[-2],0,yypvt[-0]); } break;
case 50:
# line 365 "pas.gram"
{ temp = MakeExprConst(0,-1.0);
				  yyval = BuildStmtFor(yypvt[-6],yypvt[-4],yypvt[-2],temp,yypvt[-0]); } break;
case 51:
# line 370 "pas.gram"
{ yyval = StartStmtRepeat(); } break;
case 52:
# line 372 "pas.gram"
{ yyval = BuildStmtRepeat(yypvt[-3],yypvt[-2],yypvt[-0]); } break;
case 54:
# line 379 "pas.gram"
{ yyval = 0; } break;
case 55:
# line 384 "pas.gram"
{ yyval = BuildStmtWhile(yypvt[-2],yypvt[-0]); } break;
case 56:
# line 386 "pas.gram"
{ yyval = BuildStmtWhile(0,yypvt[-0]); } break;
case 57:
# line 391 "pas.gram"
{ yyval = 0; } break;
case 58:
# line 393 "pas.gram"
{ yyval = AddCase(yypvt[-4],yypvt[-2],yypvt[-0]); } break;
case 59:
# line 395 "pas.gram"
{ yyval = AddCase(yypvt[-4],0,yypvt[-0]); } break;
case 60:
# line 397 "pas.gram"
{ yyval = yypvt[-1]; } break;
case 61:
# line 399 "pas.gram"
{ yyval = AddCase(0,yypvt[-2],yypvt[-0]); } break;
case 62:
# line 401 "pas.gram"
{ yyval = AddCase(0,0,yypvt[-0]); } break;
case 63:
# line 406 "pas.gram"
{ yyval = BuildStmtCase(yypvt[-3],yypvt[-1],0); } break;
case 65:
# line 414 "pas.gram"
{ yyerror("Use THEN instead of DO"); } break;
case 66:
# line 421 "pas.gram"
{ temp = AddToStmtList(0,0);
				  yyval = BuildStmtIf(yypvt[-2],yypvt[-0],temp); } break;
case 67:
# line 424 "pas.gram"
{ yyval = BuildStmtIf(yypvt[-4],yypvt[-2],yypvt[-0]); } break;
case 69:
# line 431 "pas.gram"
{ yyval = AppendStmtList(yypvt[-2],yypvt[-0]); } break;
case 70:
# line 436 "pas.gram"
{ temp = AddToExprList(0,0); 
				  yyval = BuildStmtProc(yypvt[-0],temp); } break;
case 71:
# line 439 "pas.gram"
{ yyval = BuildStmtProc(yypvt[-1],yypvt[-0]); } break;
case 72:
# line 444 "pas.gram"
{ yyval = BuildStmtAssign(yypvt[-2],yypvt[-0]); } break;
case 73:
# line 449 "pas.gram"
{ yyval = BuildStmtLabel(yypvt[-1]); } break;
case 74:
# line 454 "pas.gram"
{ yyval = BuildStmtGoto(yypvt[-0]); } break;
case 75:
# line 459 "pas.gram"
{ yyval = BuildStmtStmts(yypvt[-1]); } break;
case 76:
# line 464 "pas.gram"
{ yyval = AddToStmtList(0,0); } break;
case 78:
# line 471 "pas.gram"
{ yyval = AddToStmtList(0,yypvt[-0]); } break;
case 79:
# line 473 "pas.gram"
{ temp = AddToStmtList(0,yypvt[-1]);
				  yyval = AppendStmtList(temp,yypvt[-0]); } break;
case 80:
# line 476 "pas.gram"
{ yyval = AddToStmtList(0,yypvt[-0]); } break;
case 81:
# line 478 "pas.gram"
{ yyval = AddToStmtList(0,yypvt[-0]); } break;
case 82:
# line 480 "pas.gram"
{ yyval = AddToStmtList(0,yypvt[-0]); } break;
case 83:
# line 482 "pas.gram"
{ yyval = AddToStmtList(0,yypvt[-0]); } break;
case 84:
# line 484 "pas.gram"
{ yyval = AddToStmtList(0,yypvt[-0]); } break;
case 85:
# line 486 "pas.gram"
{ yyval = AddToStmtList(0,yypvt[-0]); } break;
case 86:
# line 488 "pas.gram"
{ yyval = AddToStmtList(0,yypvt[-0]); } break;
case 87:
# line 490 "pas.gram"
{ yyval = AddToStmtList(0,yypvt[-0]); } break;
case 88:
# line 492 "pas.gram"
{ yyval = AddToStmtList(0,yypvt[-0]); } break;
case 89:
# line 494 "pas.gram"
{ yyval = AddToStmtList(0,yypvt[-0]); } break;
case 90:
# line 499 "pas.gram"
{ yyval = yypvt[-1]; } break;
case 91:
# line 504 "pas.gram"
{ yyval = BuildExprConst(yypvt[-0]); } break;
case 92:
# line 506 "pas.gram"
{ yyval = BuildExprConst(yypvt[-0]); } break;
case 93:
# line 508 "pas.gram"
{ yyval = BuildExprConst(yypvt[-0]); } break;
case 94:
# line 510 "pas.gram"
{ yyval = BuildExprConst(yypvt[-0]); } break;
case 97:
# line 516 "pas.gram"
{ yyval = BuildExprFunc(yypvt[-1],yypvt[-0]); } break;
case 98:
# line 518 "pas.gram"
{ yyval = yypvt[-1]; } break;
case 99:
# line 520 "pas.gram"
{ yyval = BuildExprUnOp(NOT,yypvt[-0]); } break;
case 101:
# line 527 "pas.gram"
{ yyval = BuildExprBinOp(ASTERISK,yypvt[-2],yypvt[-0]); } break;
case 102:
# line 529 "pas.gram"
{ yyval = BuildExprBinOp(SLASH,yypvt[-2],yypvt[-0]); } break;
case 103:
# line 531 "pas.gram"
{ yyval = BuildExprBinOp(DIV,yypvt[-2],yypvt[-0]); } break;
case 104:
# line 533 "pas.gram"
{ yyval = BuildExprBinOp(MOD,yypvt[-2],yypvt[-0]); } break;
case 105:
# line 535 "pas.gram"
{ yyval = BuildExprBinOp(AND,yypvt[-2],yypvt[-0]); } break;
case 107:
# line 542 "pas.gram"
{ yyval = BuildExprBinOp(PLUS,yypvt[-2],yypvt[-0]); } break;
case 108:
# line 544 "pas.gram"
{ yyval = BuildExprBinOp(MINUS,yypvt[-2],yypvt[-0]); } break;
case 109:
# line 546 "pas.gram"
{ yyval = BuildExprBinOp(OR,yypvt[-2],yypvt[-0]); } break;
case 110:
# line 551 "pas.gram"
{ yyval = BuildExprUnOp(PLUS,yypvt[-0]); } break;
case 111:
# line 553 "pas.gram"
{ yyval = BuildExprBinOp(PLUS,yypvt[-2],yypvt[-0]); } break;
case 112:
# line 555 "pas.gram"
{ yyval = BuildExprBinOp(MINUS,yypvt[-2],yypvt[-0]); } break;
case 113:
# line 557 "pas.gram"
{ temp = BuildExprUnOp(PLUS,yypvt[-2]);
				  yyval = BuildExprBinOp(OR,temp,yypvt[-0]); } break;
case 114:
# line 563 "pas.gram"
{ yyval = BuildExprUnOp(MINUS,yypvt[-0]); } break;
case 115:
# line 565 "pas.gram"
{ yyval = BuildExprBinOp(PLUS,yypvt[-2],yypvt[-0]); } break;
case 116:
# line 567 "pas.gram"
{ yyval = BuildExprBinOp(MINUS,yypvt[-2],yypvt[-0]); } break;
case 117:
# line 569 "pas.gram"
{ temp = BuildExprUnOp(MINUS,yypvt[-2]);
				  yyval = BuildExprBinOp(OR,temp,yypvt[-0]); } break;
case 119:
# line 577 "pas.gram"
{ yyval = yypvt[-0]; } break;
case 120:
# line 579 "pas.gram"
{ yyval = yypvt[-0]; } break;
case 122:
# line 586 "pas.gram"
{ yyval = BuildExprBinOp(EQUALS,yypvt[-2],yypvt[-0]); } break;
case 123:
# line 588 "pas.gram"
{ yyval = BuildExprBinOp(NOTEQUAL,yypvt[-2],yypvt[-0]); } break;
case 124:
# line 590 "pas.gram"
{ yyval = BuildExprBinOp(LESS,yypvt[-2],yypvt[-0]); } break;
case 125:
# line 592 "pas.gram"
{ yyval = BuildExprBinOp(LSEQUAL,yypvt[-2],yypvt[-0]); } break;
case 126:
# line 594 "pas.gram"
{ yyval = BuildExprBinOp(GREATER,yypvt[-2],yypvt[-0]); } break;
case 127:
# line 596 "pas.gram"
{ yyval = BuildExprBinOp(GREQUAL,yypvt[-2],yypvt[-0]); } break;
case 128:
# line 598 "pas.gram"
{ yyval = BuildExprBinOp(IN,yypvt[-2],yypvt[-0]); } break;
case 129:
# line 603 "pas.gram"
{ yyval = AddToExprList(0,yypvt[-0]); } break;
case 130:
# line 605 "pas.gram"
{ yyval = AddToExprList(yypvt[-2],yypvt[-0]); } break;
case 131:
# line 610 "pas.gram"
{ yyval = NewExprNode(EXPRHEX); } break;
case 132:
# line 612 "pas.gram"
{ yyval = NewExprNode(EXPROCT); } break;
case 134:
# line 619 "pas.gram"
{ yyval = AddSubExpr(yypvt[-1], 0, yypvt[-0]); } break;
case 135:
# line 621 "pas.gram"
{ yyval = AddSubExpr(yypvt[-2], yypvt[-0], 0); } break;
case 136:
# line 623 "pas.gram"
{ yyval = AddSubExpr(yypvt[-3], yypvt[-1], yypvt[-0]); } break;
case 137:
# line 625 "pas.gram"
{ yyval = AddSubExpr(yypvt[-4], yypvt[-2], yypvt[-0]); } break;
case 138:
# line 630 "pas.gram"
{ yyval = AddToExprList(0,yypvt[-0]); } break;
case 139:
# line 632 "pas.gram"
{ yyval = AddToExprList(yypvt[-2],yypvt[-0]); } break;
case 140:
# line 637 "pas.gram"
{ yyval = BuildExprSym(yypvt[-0]); } break;
case 141:
# line 639 "pas.gram"
{ yyval = BuildExprSubscript(yypvt[-3],yypvt[-1]); } break;
case 142:
# line 641 "pas.gram"
{ yyval = BuildExprDeref(yypvt[-1]); } break;
case 143:
# line 643 "pas.gram"
{ yyval = BuildExprDot(yypvt[-2],yypvt[-0]); } break;
case 144:
# line 650 "pas.gram"
{ DefineVarList(yypvt[-3],yypvt[-1],VAR,0,0,0); } break;
case 148:
# line 664 "pas.gram"
{ yyval = PointerForwardType(yypvt[-0],ATPASCAL); } break;
case 149:
# line 669 "pas.gram"
{ yyval = SetType(yypvt[-0],0); } break;
case 150:
# line 671 "pas.gram"
{ yyval = SetType(yypvt[-0],0); } break;
case 151:
# line 677 "pas.gram"
{ yyval = MakeConstSet(yypvt[-0],0); } break;
case 152:
# line 682 "pas.gram"
{ yyval = AddToConstSetList(0,yypvt[-0]); } break;
case 153:
# line 684 "pas.gram"
{ yyval = AddToConstSetList(yypvt[-2],yypvt[-0]); } break;
case 154:
# line 689 "pas.gram"
{ yyval = MakeExprSet(yypvt[-0],0); } break;
case 155:
# line 691 "pas.gram"
{ yyval = MakeExprSet(yypvt[-2],yypvt[-0]); } break;
case 156:
# line 696 "pas.gram"
{ yyval = AddToExprSetList(0,yypvt[-0]); } break;
case 157:
# line 698 "pas.gram"
{ yyval = AddToExprSetList(yypvt[-2],yypvt[-0]); } break;
case 158:
# line 703 "pas.gram"
{ yyval = 0; } break;
case 159:
# line 705 "pas.gram"
{ yyval = MakeVariant(yypvt[-4],yypvt[-1]); } break;
case 160:
# line 707 "pas.gram"
{ yyval = MakeVariant(0,yypvt[-1]); } break;
case 161:
# line 712 "pas.gram"
{ yyval = EmptyFieldList(); } break;
case 162:
# line 714 "pas.gram"
{ yyval = MakeFieldList(yypvt[-2],yypvt[-0]); } break;
case 163:
# line 719 "pas.gram"
{ yyval = AddToVariantList(0,yypvt[-0]); } break;
case 164:
# line 721 "pas.gram"
{ yyval = AddToVariantList(yypvt[-2],yypvt[-0]); } break;
case 165:
# line 726 "pas.gram"
{ temp = EmptyFieldList();
				  yyval = MakeVariant(0,temp); } break;
case 166:
# line 732 "pas.gram"
{ temp = TypeOf(yypvt[-3]);
				    yyval = MakeTagField(0,temp,yypvt[-1],yypvt[-0]); } break;
case 167:
# line 735 "pas.gram"
{ temp = TypeOf(yypvt[-3]);
				    yyval = MakeTagField(yypvt[-5],temp,yypvt[-1],yypvt[-0]); } break;
case 168:
# line 741 "pas.gram"
{ yyval = AppendFieldList(yypvt[-1],yypvt[-0]); } break;
case 171:
# line 750 "pas.gram"
{ yyval = AppendFieldList(yypvt[-2],yypvt[-0]); } break;
case 172:
# line 755 "pas.gram"
{ yyval = RecordType(yypvt[-1],0); } break;
case 173:
# line 757 "pas.gram"
{ yyval = RecordType(yypvt[-1],0); } break;
case 174:
# line 762 "pas.gram"
{ yyval = FileType(yypvt[-0]); } break;
case 175:
# line 764 "pas.gram"
{ yyval = FileType(yypvt[-0]); } break;
case 176:
# line 769 "pas.gram"
{ yyval = ArrayType(yypvt[-1],yypvt[-0],ARRAY,0); } break;
case 177:
# line 771 "pas.gram"
{ yyval = ArrayType(yypvt[-1],yypvt[-0],ARRAY,0); } break;
case 178:
# line 777 "pas.gram"
{ yyval = yypvt[-0]; } break;
case 179:
# line 780 "pas.gram"
{ yyval = ArrayType(yypvt[-1],yypvt[-0],ARRAY,0); } break;
case 180:
# line 785 "pas.gram"
{ yyval = SubrangeType(yypvt[-2],yypvt[-0],0); } break;
case 181:
# line 790 "pas.gram"
{ temp = MakeIdent(yypvt[-0]);
				    yyval = AddToIdentList(0,temp); } break;
case 182:
# line 794 "pas.gram"
{ temp = MakeIdent(yypvt[-0]);
				    yyval = AddToIdentList(yypvt[-2],temp); } break;
case 183:
# line 797 "pas.gram"
{ yyval = yypvt[-1];
				  yyerror("Identifier expected"); } break;
case 184:
# line 800 "pas.gram"
{ temp = MakeIdent(yypvt[-0]);
				    yyval = AddToIdentList(yypvt[-1],temp);
				    yyerror("Comma expected"); } break;
case 185:
# line 807 "pas.gram"
{ yyval = MakeEnumNode(yypvt[-0],0); } break;
case 186:
# line 812 "pas.gram"
{ yyval = AddToEnumList(0,yypvt[-0]); } break;
case 187:
# line 815 "pas.gram"
{ yyval = AddToEnumList(yypvt[-2],yypvt[-0]); } break;
case 188:
# line 817 "pas.gram"
{ yyval = yypvt[-1];
				  yyerror("Identifier expected"); } break;
case 189:
# line 820 "pas.gram"
{ yyval = AddToEnumList(yypvt[-1],yypvt[-0]);
				  yyerror("Comma expected"); } break;
case 190:
# line 826 "pas.gram"
{ yyval =  EnumerationType(yypvt[-1]); } break;
case 191:
# line 831 "pas.gram"
{ yyval = TypeOf(yypvt[-0]); } break;
case 200:
# line 855 "pas.gram"
{ DefineType(yypvt[-3],yypvt[-1]); } break;
case 201:
# line 857 "pas.gram"
{ yyerror("Use '=' instead of ':' for type declarations"); } break;
case 202:
# line 859 "pas.gram"
{DefineType(yypvt[-4],yypvt[-1]); } break;
case 206:
# line 874 "pas.gram"
{ yyval = BuildExprSet(yypvt[-1],0); } break;
case 207:
# line 876 "pas.gram"
{ yyval = BuildExprSet(0,0); } break;
case 208:
# line 881 "pas.gram"
{ yyval = 0; } break;
case 211:
# line 891 "pas.gram"
{ yyval = ConstUnOp(PLUS,yypvt[-0]); } break;
case 212:
# line 893 "pas.gram"
{ yyval = ConstUnOp(MINUS,yypvt[-0]); } break;
case 214:
# line 900 "pas.gram"
{ yyval = ConstSym(yypvt[-0]); } break;
case 218:
# line 911 "pas.gram"
{ DefineConst(yypvt[-3],yypvt[-1]); } break;
case 220:
# line 918 "pas.gram"
{ DefineLabel(yypvt[-0]); } break;
case 221:
# line 920 "pas.gram"
{ yyerror("Comma expected");
				  DefineLabel(yypvt[-0]); } break;
case 222:
# line 923 "pas.gram"
{ DefineLabel(yypvt[-0]); } break;
case 225:
# line 936 "pas.gram"
{ temp = MakeIdent(yypvt[-0]);
				    yyval = AddToIdentList(0,temp); } break;
case 226:
# line 941 "pas.gram"
{ ScanEofOK() ; } break;
case 229:
# line 948 "pas.gram"
{ yyerror("Program must end with a period."); } break;
case 230:
# line 951 "pas.gram"
{ yyerror("Program must end with a period."); } break;
case 232:
# line 958 "pas.gram"
{ yyerror("Missing semi-colon"); } break; 
		}
		goto yystack;  /* stack new state and value */

	}

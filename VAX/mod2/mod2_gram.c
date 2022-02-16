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

# line 176 "mod2.gram"
#include <stdio.h>
#define YYMAXDEPTH	500
/* standard type pointers, globally defined in symtab */
#ifdef vms
globalref int anyTypeNode, procTypeNode;
#else unix
extern int anyTypeNode, procTypeNode;
#endif
static int temp;

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
-1, 9,
	256, 7,
	-2, 4,
-1, 107,
	8, 332,
	23, 332,
	-2, 334,
-1, 179,
	26, 117,
	42, 117,
	43, 117,
	44, 117,
	69, 117,
	-2, 124,
-1, 228,
	256, 24,
	-2, 21,
-1, 231,
	26, 120,
	42, 120,
	43, 120,
	44, 120,
	69, 120,
	-2, 124,
-1, 290,
	256, 51,
	-2, 49,
-1, 665,
	7, 379,
	11, 379,
	-2, 230,
	};
# define YYNPROD 390
# define YYLAST 1327
short yyact[]={

  21, 169, 729,  70, 700, 116, 536, 208, 540, 699,
 206, 207, 610, 504, 136, 352, 599, 683, 634,  72,
 539, 450, 348, 585, 178, 701, 361, 115,  97, 343,
 342, 529, 340, 295, 548, 239, 235, 167, 461, 250,
 181,  33, 360,   9,   6, 284, 480, 280, 298,   5,
 399, 277, 170, 182, 210,  32,  35,  66, 232, 270,
 180, 254, 107, 552, 179, 100,  34,  67, 449, 719,
 293,  30, 167, 167, 167, 215,  30, 106, 591, 608,
 297, 197, 167, 436, 101, 298, 197, 735, 703, 203,
 516, 201, 541, 196, 203,  16, 201, 200, 196, 286,
 287, 592, 200, 531, 532, 533, 534, 199, 193, 285,
 542, 114, 199, 193, 535, 198, 202, 297, 135, 587,
 198, 202, 564, 658,  64, 527, 653, 377, 652, 216,
 217, 219, 632, 107, 353,  17,  18, 380, 218, 607,
 543, 544, 537, 538,  28, 565,  31, 611, 528, 166,
 600,  94, 147,  30, 606, 601, 152, 204, 174, 354,
 486,  30,  22,  23,  24,  25, 645, 590, 586,  30,
 220, 353,  30, 531, 532, 533, 534, 209, 530, 197,
 133, 204, 121,  98, 535, 124,  30, 203, 351, 201,
  29, 196, 146, 460, 249, 200, 354, 249, 249, 249,
 224, 603, 604, 204, 245, 199, 193, 227, 162,  30,
 150, 469, 225, 198, 202, 177, 486, 283, 603, 604,
 613, 156,   8, 296, 157, 223, 331, 332, 310, 289,
 204, 204, 306, 350, 176, 763, 173, 249, 249, 308,
 229, 163, 758, 594, 469, 231, 165, 249, 249, 249,
   4, 249, 612, 594, 158, 290, 159, 141, 349, 257,
 140, 204, 204, 333, 757, 275, 299, 469, 746, 249,
 314, 315, 249, 249, 288, 355, 281, 334, 656, 359,
 278, 415, 635, 339, 141, 356, 357, 140, 655, 686,
  73, 726, 316, 386, 376, 378, 379, 643, 388, 581,
 194, 639, 358, 299, 594, 194, 391, 392, 171, 517,
 393, 394, 346, 167, 514, 366, 513, 204, 249, 249,
 249, 249, 249, 249, 249, 249, 249, 249, 395, 506,
 507, 505, 249, 249, 249, 249, 249, 249, 249, 595,
 437, 154, 730, 375, 716, 300, 249, 685,  30, 204,
  73, 396, 249, 204, 398, 397, 204, 314, 731, 249,
 204, 195, 167, 508, 503, 337, 165, 416, 417, 418,
 419, 457, 420, 430, 717, 457, 718, 434, 428, 510,
 435, 509, 432,  57, 439, 195, 475, 331, 332, 478,
 702, 484, 336, 265, 549, 331, 332, 296, 194, 264,
 490, 489, 492,  51,  52, 367, 311, 263, 167, 267,
  56, 249, 249, 249, 249, 249, 249, 266, 541, 493,
 438, 309, 481, 305, 333, 479, 304, 249, 249, 485,
 467, 468, 333, 307, 195, 195, 614, 249, 334, 303,
  53, 249, 412, 249, 302, 249, 334,  30, 301, 496,
 335, 616, 499,  68,  54, 549, 500, 167,  89, 262,
 238, 457, 221,  71,  14, 195, 195, 518, 228, 211,
 234, 457, 168, 221, 457, 457, 118, 457, 457, 103,
  11, 222,  62, 561, 246,   7, 248, 598, 484, 484,
 126, 296, 222, 502, 126,  51,  52, 545, 515, 572,
 562,  30, 240, 241, 242, 243, 710, 204, 249, 249,
 125, 204, 204, 204, 425, 108, 563, 249, 569, 424,
 571, 195,  51,  52, 457, 457, 485, 485, 588, 526,
 247, 573,  53, 570, 128, 577, 578, 579, 483, 575,
 127, 111, 602, 457, 596, 597,  54, 484, 110, 589,
  86, 728, 440, 195, 255, 457, 620, 195, 457,  53,
 195, 457, 593, 709, 195, 484, 630, 134, 445, 574,
 633,  36, 423,  54, 636, 109, 733,  55, 580, 221,
 112, 221, 615, 148, 149, 485, 108, 457, 457, 629,
 637, 649, 457, 560, 732, 557, 644, 640, 222, 457,
 222, 602, 221, 485, 708, 660, 642, 457, 457, 457,
 221, 605, 650, 484, 484, 654, 488, 659, 221, 623,
 624, 222, 626, 627, 487, 426, 512, 675, 676, 222,
 678, 679, 477, 484, 473, 566, 567, 222, 442, 641,
 689, 204, 204, 204, 684, 204, 249, 101, 681, 671,
 674, 485, 485, 457, 457, 688, 648, 440, 292, 130,
 646, 602, 602, 602, 687, 690, 691, 692, 484, 693,
 447, 485, 696, 441, 471, 663, 664, 704, 705, 706,
 707, 711, 372, 618, 712, 331, 332, 370, 720, 677,
 722, 427, 680, 113, 721, 371, 713, 328, 330, 553,
 617, 368, 723, 714, 725, 221, 485, 694, 724, 736,
 727, 195, 631, 734,  20, 195, 195, 195, 347, 476,
 738,  10, 333, 546, 222, 742, 741, 739, 204, 740,
 744, 594, 221, 737,  71, 747, 334, 751, 748, 749,
 750, 753, 667, 743,  63, 752, 474, 444, 233, 755,
 283, 222, 745, 258, 619, 759, 760, 668, 762, 522,
 669, 670, 443, 764, 221, 221,  90,  91,  92,  93,
 551,  60, 521, 555, 556, 525, 558, 559, 390, 374,
 682, 389,  71, 222, 222, 132, 408, 409, 400, 401,
 402, 403, 404, 405, 406, 407, 387, 421, 422, 385,
 131, 338, 251, 344, 236, 237, 426,  51,  52,  17,
  18, 252,  60, 246, 426, 248, 212, 214,  28, 253,
  31, 382, 446, 362, 583, 381, 365, 362, 511, 112,
  30, 240, 241, 242, 243, 102,  22,  23,  24,  25,
 144, 373, 145, 129,  53, 195, 195, 195,  28, 195,
  31,  60, 236, 237,  61, 369,  58, 625,  54, 247,
 628, 246, 182, 248,  29,  30,  22,  23,  24,  25,
 112, 494, 495, 143, 497, 498, 413, 414,  30, 240,
 241, 242, 243, 410, 411, 117, 647, 276, 236, 237,
  49, 651, 325, 326,  29,  87,  88, 246, 657, 248,
 344,  47, 523,  84,  85, 105, 344, 247, 666,  59,
 524,  26, 236, 237,  30, 240, 241, 242, 243, 236,
 237, 246, 550, 248,  45,  46, 554,  27, 246, 470,
 248,  60, 195, 279, 271, 268, 345, 269,  30, 240,
 241, 242, 243, 247, 458,  30, 240, 241, 242, 243,
 547, 459, 697, 698, 451, 452, 453, 236, 237, 454,
 455, 609,  17,  18, 282, 205, 246, 247, 248, 329,
 327,  28, 274,  31, 247, 271, 272, 244, 273, 584,
 364, 344, 501,  30, 240, 241, 242, 243, 102,  22,
  23,  24,  25,  17,  18, 362,  19, 519, 192, 520,
 183, 184,  28, 230,  31,  60, 185, 622, 271, 268,
 186, 269, 247, 431,  48,  50, 187,  29, 260,  30,
  22,  23,  24,  25, 188, 190, 261, 582, 467, 468,
 317, 318, 320, 322, 319, 321, 323, 189, 191, 164,
 138, 160,  82,  83, 155,  30, 482, 291,  29, 467,
 468, 294, 460,  95,  96, 213, 384, 661, 662, 256,
 383, 161, 137, 344,  65, 139,  30, 715, 324, 638,
 313, 672, 673, 460, 756, 312, 153, 465, 466, 754,
 463, 119, 120, 464, 122, 123, 761, 491, 175, 568,
 172, 472, 456, 151,  69, 142,  15,  13, 465, 466,
 695, 463, 462, 469, 464, 467, 468, 576, 226, 467,
 468,  12, 472, 456,  37,  38,  40,  42,  39,  41,
  43,   3, 665, 462, 469,   2,  30,   1,   0, 460,
   0,   0,   0, 460,  74,  75,  76,  77,  78,  79,
  80,  81,   0, 433,   0,   0,   0,   0,   0,   0,
   0,   0,  44,   0, 465, 466,   0, 463, 465, 466,
 464, 463,   0,   0, 464,   0,   0, 429, 472, 456,
   0,   0, 448, 456, 341,   0, 236, 237,   0, 462,
 469,   0,   0, 462, 469, 246,   0, 248,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0, 363,  17,
  18,   0,  30, 240, 241, 242, 243,   0,  28,   0,
  31,   0, 259,  17,  18,   0,   0,  99,   0,   0,
   0, 104,  28,   0,  31,  30,  22,  23,  24,  25,
   0, 247,   0,   0,   0,  17,  18,   0,   0,  30,
  22,  23,  24,  25,  28,   0, 621,   0,  65, 236,
 237,   0,   0,   0,  29,   0,   0,   0, 246,   0,
 248,  30,  22,  23,  24,  25,   0,   0,  29,   0,
   0,  17,  18,   0,   0,  30, 240, 241, 242, 243,
  28,   0,  31,   0,   0,   0,   0,   0,   0,   0,
  29,   0,   0,   0,   0,   0,   0, 102,  22,  23,
  24,  25,   0,   0, 247,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,  29 };
short yypact[]={

  -6,-1000,-1000,-1000,-1000, 458, 167, 710, 453,-1000,
1212, 710, -26,-200, 547,1100, 923, 838, 838, 519,
 377, 844,-1000,-1000,-1000,-1000,-1000,-1000,1212, 838,
-1000, 992,-1000, 725,-1000, 246,-1000,1212,1212,1212,
1212,1212,1212,1212,1212, 838, 838, 902, 492, 894,
 400, 838, 838, 838, 838, 838, 838, 838, 961,-1000,
 452,1198, 552,-1000, 523, 516, 862,-1000, 672, -26,
-1000,-1000,-1000, 449,-1000,-1000,-1000,-1000,-1000,-1000,
-1000,-1000, 804, 804, 838, 838, 838, 838, 838, 838,
-1000,-1000,-1000,-1000,-1000,-1000,-1000, 485, 515, 509,
-1000, 821, 637,-1000,-1000, 777,-1000, 844,-1000,-1000,
-1000,-1000,1212,1212, 725, 209,-1000, 833,-1000, 804,
 804,-1000, 804, 804,-1000,-1000,1270,-1000,-1000,1212,
1212,-1000,1212,-1000,-1000,-1000, 246, 302, 186,-1000,
 335, 445,  52,-1000,-1000,-1000,-1000, 821,-1000,-1000,
-1000, 197,-1000, 184, 160,-1000,  44,-1000,-1000,-1000,
-1000,-1000,-202, 442, 807,  48, 454,-1000, 174,-1000,
-1000,-1000,-1000, 157,-1000,-1000, 152, 441,-1000, 853,
  49,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000,
-1000,-1000,-1000,1248,-1000, 797, 803, 803, 956,-1000,
-1000, 432, 321, 346, 998, 965,  24,  20,  18, 246,
-1000, 710,-1000, 636,  47, 314, 421, 417, 412, 399,
-1000, 396,-1000, 430, 209, 394,  52, 379,-1000,  49,
 142, 853,-1000,-1000,1016, 891, 474, 474, 392, 359,
-1000,-1000,-1000,-1000,-1000,-1000,1248, 474, 918, 924,
 803,-1000,-1000, 704, 192,-1000,-1000, 131,  93,  93,
  44,  44, 797,  93,-1000,-1000,-1000,-1000,1248,-1000,
-1000,1175,1248,-1000, 378,-1000,-1000, 687, 846,-1000,
 673, 832,-1000, 757, 312,  45, 335,  46, 816, 812,
-1000, 790, 321,-1000, 773,-1000, 756, 335, 335,-1000,
-1000,-1000,-1000,-1000,-1000,-1000, 454, 725, 246, -26,
-1000, 710, -26,-206,-1000,-1000,  49,1248,1248,1248,
1248,1248,1248,1248,1248, 474, 474, 882, 384, 875,
 223, 474, 474, 474, 474, 474, 474, 474, 549,-1000,
 494, 489, 798,-1000, 670, 911,-1000,-1000,  44,-1000,
-1000, 887,  44,-1000,-1000,  44,  14, 296, 803,  44,
 649,-1000, 616,-1000, 739,-1000, 544,-1000,1212,-1000,
1099,-1000,-1000,-1000,1039, 612, 724, 335, 697, 610,
 335,-1000,-1000, -26,-210,-1000, 805,-1000,  10,-1000,
 126, 602, 594,-1000,-1000,-1000, 725,-1000, 725, 246,
-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000, 682, 682,
 474, 474, 474, 474, 474, 474,-1000,-1000,-1000,-1000,
-1000,-1000,-1000,-1000,-1000,-1000,1248,1248, 468, 339,
 287, 337, 806, 604, 272, 270, 803,-1000,  23, 265,
1248,-1000,1248,-1000,1248,-1000, 763, 750,1212,-1000,
-1000,-1000,-1000,-1000,-1000,-1000,1212, 764,-1000,-1000,
 420,  68,  98,  55,  53,  28, 713, 367,1212,-1000,
1039, 685,1212,1039,1039, 573,1039,1039, 571, 725,
 246,-1000,  10,-1000, 805,  65,-1000, 126, 126,-1000,
-1000, -26,-1000,-1000, 682, 682,-1000, 682, 682,-1000,
-1000,-1000,-1000,-1000,-1000,-1000,  44, 803, 851,-1000,
  44,  44,  44,-1000,-1000,-1000, 803,-1000,-1000,-1000,
-1000,-1000,-1000,1018,1039,1212, 111, 159, 110,  21,
  28,-1000,-1000,-1000,-1000,-1000, 295, 381, 381,-1000,
 465, 128, 420,  97,  82,  12, 182, 428,-1000, 686,
 662, 745, 725,1234,1039, 685, 685,1039, 685, 685,
1039,-1000, 725,-1000, 126,  75,-1000,-1000,  52, 236,
 725, 236,-1000, 257, 192, 617, 584, 253,-1000,-1000,
 130,-1000,-1000,-1000, 639,-1000,1039, 420, 805,-1000,
 159,1039,  71,  69, 381,-1000, 244, 234,1039,  66,
 145, 583, 805,1212,1212,-1000, 420, 420,1095, 734,
-1000,-1000, 126, 126, 367,-1000,-1000,1212,1212,-1000,
-1000, 808,-1000, 725, 725, 685, 725, 725, 685, 236,
-1000,-1000, 126,-1000,-1000, 286,-1000,-1000,  52,-1000,
  44,  44,  44,-1000,  44, 803,1212,-1000, 111, 805,
-1000,-1000,1039,1039,-1000,-1000,-1000,-1000, 134,  31,
 145, 145, 145,-1000,-1000,-1000,-1000, 182, 582,-1000,
-1000,-1000,-1000, 539, 481,-1000,-1000, 725,-1000,-1000,
 725,-1000,-1000, 306, 454, 335, 725, 209, 306,-1000,
 287,-1000,-1000, 247,  93, 527,-1000,-1000,-1000, 316,
-1000, 572, 554, 134,  30,-1000,-1000,-1000, 321,-1000,
-1000,-1000,-1000, 246,-1000,-1000,-1000,-1000,-1000,-1000,
-1000, 454,-1000, 246,-1000,-1000,-1000,  44,-1000, 224,
 134, 381, 381, 381, 316, 134, 805, 725,-1000,  24,
  20,  18,-1000,-1000,-1000, 220,-1000,-1000, 722, 722,
 722, 198, 316,-1000,  52,-1000,  52,-1000,-1000, 191,
-1000,  52,-1000,-1000,-1000 };
short yypgo[]={

   0,1127,1125,1121,  43,1111,  41,   3,  27,  14,
  19,   5,1108,   1,1097,1095,1094,1093,1090,1089,
1088,1087,1086,  18,  17,1079,1076,1075,1074,1070,
1069,1067,  10,  11,   7,  69,1065,   8,1062, 453,
1061,1060,1056, 147,   0,  38,  33,1055,1051,1047,
1046,1044,1041,1040,  24,1039,  15,1038, 204,1037,
1026,  39,1025,  61,1024,1018, 554,1016,1013,  30,
1010,  22,1006,  13,  64,  60,  40,1003,1001,1000,
 998,  59, 980,  35, 977, 460,  36, 970, 969, 470,
  26,  42, 965, 964, 299,  63, 961,  12, 960,  31,
 959, 956,  68,  67,  25,  29,   4,   6,  16,  20,
   9,   2, 955, 954,  21,  23, 951,  34, 950, 944,
 933, 929,  28, 927,  65, 911, 124,  32, 909, 905,
  77, 714, 996,  95, 901, 890,1096, 887, 885, 873 };
short yyr1[]={

   0,   1,   1,   1,   5,  12,   2,  14,  15,   2,
  13,  13,  16,  17,  19,   3,  21,  22,  20,  25,
  18,  27,  28,  26,  29,  30,  26,  31,  31,  31,
  31,  24,  24,  36,  36,  36,  38,  38,  38,   8,
   8,  23,  23,  23,  23,   4,   4,   6,   6,  41,
  40,  42,  40,  43,  43,  43,  46,  46,  46,  46,
  47,  47,  49,  49,  48,  48,  50,  48,  51,  51,
  51,  51,  51,  53,  53,   9,   9,  55,  55,  55,
  55,  55,  55,  35,  35,  52,  52,  56,  56,  57,
  60,  59,  61,  61,  61,  62,  62,  65,  64,  63,
  63,  67,  67,  68,  68,  68,  68,  68,  68,  70,
  70,  71,  71,  72,  73,  73,  73,  54,  54,  54,
  54,  54,  54,  74,  77,  74,  74,  75,  75,  78,
  79,  80,  80,  80,  80,  80,  76,  76,  76,  76,
  76,  76,  76,  76,  76,  76,  76,  76,  76,  81,
  81,  83,  83,  83,  83,  83,  83,  83,  83,  85,
  85,  85,  85,  85,  85,  85,  86,  86,  86,  86,
  87,  87,  87,  87,  88,  88,  88,  88,  89,  89,
  89,  66,  66,  66,  66,  66,  66,  66,  66,  66,
  90,  90,  91,  91,  82,  82,  92,  92,  92,  92,
  92,  92,  92,  58,  58,  93,  93,  93,  93,  93,
  93,  93,  93,  34,  34,  96,  96,  96,  97,  97,
  97,  98,  98,  98,  99,  99,  99,  99,  99,  99,
 100, 100, 101, 101, 101, 103, 103, 104, 104, 105,
 105,  69,  69, 106, 106, 106, 108, 108, 108, 109,
 109, 109, 109, 109, 110, 110, 111, 111, 107, 107,
 112, 112, 112,  45,  45, 113, 113, 113, 114, 114,
 114, 114, 114, 115, 115, 116, 116,  37,  37,  37,
  37, 117, 117, 118, 118, 118, 118, 119, 102, 102,
 102,  94,  94,  94,  94,  94,  94,  94,  94,  94,
 120, 121, 120, 120, 120, 120,  33,  33,  95,  95,
  95, 123, 124, 124, 122, 122, 125, 125, 125, 125,
 126, 126,  84,  84,  84,  84, 127, 127, 128, 128,
 129, 129, 130, 130, 131, 131, 131, 131, 131, 131,
 131, 131, 131, 131, 132, 132, 132, 132, 132, 132,
 132, 133, 133, 133, 133, 134, 134, 134, 134, 135,
 135, 135, 135, 136, 136, 136,  39,  39,  39,  39,
  39,  39,  39,  39,  39, 137, 137,  32,  32,  44,
  44,  10,  10, 138,  11, 139, 139, 139,   7,   7 };
short yyr2[]={

   0,   1,   1,   1,   0,   0,  12,   0,   0,   9,
   1,   1,   0,   0,   0,  15,   0,   0,  13,   0,
  12,   0,   0,  13,   0,   0,   9,   2,   2,   2,
   1,   0,   2,   3,   5,   5,   1,   2,   3,   0,
   2,   3,   4,   3,   0,   0,   3,   0,   1,   0,
  11,   0,   7,   1,   3,   4,   3,   4,   4,   1,
   2,   3,   0,   2,   1,   3,   0,   4,   2,   2,
   2,   1,   1,   0,   2,   1,   3,   2,   3,   3,
   3,   3,   3,   2,   4,   4,   2,   1,   1,   5,
   0,   4,   1,   1,   2,   9,  11,   0,   5,   1,
   1,   5,   5,   0,   5,   5,   2,   3,   3,   5,
   7,   1,   1,   5,   1,   3,   5,   1,   3,   2,
   2,   1,   0,   3,   0,   3,   1,   1,   2,   1,
   3,   1,   2,   2,   2,   2,   1,   1,   1,   1,
   1,   1,   1,   1,   1,   1,   1,   2,   1,   2,
   3,   1,   1,   1,   1,   1,   1,   3,   2,   1,
   3,   3,   3,   3,   3,   3,   1,   3,   3,   3,
   1,   3,   3,   3,   1,   3,   3,   3,   1,   2,
   2,   1,   3,   3,   3,   3,   3,   3,   3,   3,
   1,   3,   1,   3,   1,   3,   4,   4,   2,   2,
   3,   2,   2,   1,   1,   5,   6,   6,   6,   6,
   7,   7,   2,   0,   2,   0,   1,   3,   1,   2,
   2,   1,   4,   6,   0,   1,   1,   1,   1,   1,
   4,   4,   3,   4,   4,   1,   3,   1,   3,   1,
   3,   1,   3,   0,   3,   3,   1,   3,   3,   0,
   3,   6,   7,   8,   1,   3,   0,   2,   1,   3,
   3,   4,   4,   1,   1,   4,   5,   5,   3,   3,
   4,   3,   4,   2,   3,   5,   6,   1,   3,   2,
   2,   1,   3,   1,   3,   2,   2,   3,   1,   1,
   1,   1,   1,   1,   1,   1,   1,   1,   3,   3,
   4,   0,   5,   2,   5,   2,   0,   2,   0,   4,
   2,   4,   3,   3,   1,   3,   3,   3,   4,   4,
   0,   1,   3,   3,   4,   4,   0,   1,   2,   3,
   1,   3,   1,   1,   1,   1,   1,   1,   1,   1,
   1,   3,   2,   2,   1,   3,   3,   3,   3,   3,
   3,   1,   3,   3,   3,   1,   3,   3,   3,   1,
   3,   3,   3,   1,   2,   2,   1,   3,   3,   3,
   3,   3,   3,   3,   3,   4,   2,   0,   2,   1,
   3,   2,   1,   0,   2,   1,   0,   1,   1,   0 };
short yychk[]={

-1000,  -1,  -2,  -3, 256,  55,  50,  27,  55,  -4,
  11,  27,  -5, -14, -39,-136,-133,   1,   2,-132,
-131, -44,  28,  29,  30,  31,-125,-123,  10,  56,
  27,  12,  -4,  -6,  92, 256,  24,  14,  15,  18,
  16,  19,  17,  20,  52,   1,   2,-134,-132,-135,
-132,   3,   4,  40,  54,  58,  33,   6,  12,-128,
   7,  10, -39,-131,-126, 256,-104,-103, -39, -16,
  -7,   9, -10,  44,-136,-136,-136,-136,-136,-136,
-136,-136,-132,-132,   1,   2,  58,   1,   2,  58,
-131,-131,-131,-131,-133,-132,-132,-122,-126, 256,
-124,-104,  27,  27,  23,-129,-130, -44, -39,  23,
  25,  25,   8,  21,  -6,  -8, -11,-138,  27,-132,
-132,-133,-132,-132,-133,  25,   9,  25,  25,  22,
  22,  23,   8,-103, -39,  -7,  -9, -38, -53, -36,
  51,  48, -15,-139,   7,   9,-124,-104, -39, -39,
-130, -17, -10, -26,  39, -51,  35,  38,  68,  70,
 -52, -40, -35,  55, -55,  60, -37,  27,  27, -13,
   0, 256, -18,  39, -11, -20,  50,  55, -54, -74,
 -75, -76,   9, -79, -78, -72, -70, -67, -64, -59,
 -62, -57, -80,  64, 256, -58,  49,  37,  71,  63,
  53,  47,  72,  45, -44, -92, -32, -33, -34,  -9,
 256,  27,   9, -47,  10,  27,  81,  82,  90,  83,
  -7,   8,  27,  51,  -8,  55, -12,  55,  27, -75,
 -77, -74,   9, -66, -89, -86,   1,   2, -85, -83,
  28,  29,  30,  31, -84, -58,  10,  56,  12, -44,
 -61,   5,  14,  22, -63, -66, 256, -63, -66, 256,
 -65, -60,  27, -58,  53,  47,  71,  63,  11,  13,
 -81,  10,  11,  13,   7, -81,-137,  27, 256,-120,
  27, 256, -93, -37,  27,  91,  81,  82, 256, -10,
  -4, -49,  22,  23, -48, -46, -37,  70,  38, 256,
  31,  27,  27,  27,  27,  27, -37,   3,  -9,  27,
 -13,  27, -27, -29, -76, -76, -75,  14,  15,  18,
  16,  19,  17,  20,  52,   1,   2, -87, -85, -88,
 -85,   3,   4,  40,  54,  58,  33,   6, -66, -83,
-127, 256, -69,-105, -66,  12, -63,  14, -71,  66,
  41,  57, -56,  41,  66, -56, -54, -54, -61, -56,
 -91, -90, -66,  23, -82, -66, -91,  27,  14,   9,
  14,  22,   9,   9,  22,  31, -37,  82, -37, -37,
  91,   9,   9, -41, -42,   9, -44,  23,  -7,   8,
  22, -37, -37,  -7,  -7, -10,  -6,  -4,  -6, 256,
 -89, -89, -89, -89, -89, -89, -89, -89, -85, -85,
   1,   2,  58,   1,   2,  58, -83, -83, -83, -83,
 -86, -85, -85,  23,  25,  25,   8,  21,-127, 256,
 -54, -68, -69, 256, -54, -54,  69,  44, -63, -54,
   8,  24,  22,  23,   8,  24, -39, -94,  73,-102,
-114,-113,-112,-101,-100, -98,  74, -44,-119,-116,
  34, -45,  84,  62,  65,  59,  60,  10,  11,  85,
-121, -94,  73,  22,  22, -37,  22,  22, -37,  -6,
 256, -46, -50, -43, -44, -45,  34,  22,  22, -11,
  -7, -21,  -7, -10, -85, -85, -86, -85, -85, -86,
-105, -66,  25,  25, -73,  44,  42,  43,  26,  44,
  42,  22,  22,  44,  44, -63,  67,  44, -90, -66,
 -66,   9,   9, -39, -39,  11,-102,  57,  80, -99,
  80,  75,  76,  77,  78,  86,-107,  87,  88,-109,
 -37,  37,  57,  87,  88, -99,  10,-118,-117,  27,
 -39, -94, -95,  14, -39, -94, -94,  22, -94, -94,
  22,  -7, -10, -46,  57,  80, -43, -43, -19,  -8,
  -6,  -8, -11, -54, -63, -69, 256, -54, -54, -54,
 -63, -94,   9, -94, -39,-115,  57,   8, -44,-114,
  57,  57,  80, -99,   9,  44,-107,-107,  22,-108,
  22,  27, -44,  73,  74,-102,  57,  57,  67, -96,
 -97, -43,  70,  38,   8,-117,  23,  14,  21,   9,
  -7,  12, -39, -95, -95, -94, -95, -95, -94,  -8,
  -7, -43,  57, -13, -23,  46,  -7, -23, -30,  44,
 -71,  22,  22,  44, -56,  36,  21, -94,-102, -44,
-114, -94,  57,  57,-109,  44,  44, -94,  57,-108,
  22, -39, -39,-102,-102,  27, -94,   8,  23, -43,
 -43,-117, -39, -39,-122,  -7,  -7, -95,  -7,  -7,
 -95, -23, -43, -24, -37,  61,   3,  -8, -24, -13,
 -54, -54, -54, -54, -63, -39,-115, -94, -94,-110,
-106,-104, 256,  57,-108,-108,-108, -97,  22,  24,
  25,  -7,  -7,  -9, -10, -31,  38,  68,  70, -35,
  -7, -37,  -7,  -9, -10, -73,  44, -56,  24,-111,
  26,  42,  22,  22,-110,  57, -44, -10, -11, -32,
 -33, -34,  -7, -10, -11, -54,  44,-106,-107,-107,
-107,-111,-110,  -7, -25, -11, -28,  44,  44,-111,
 -13, -22, -13,  44, -13 };
short yydef[]={

   0,  -2,   1,   2,   3,   0,   0,  45,   0,  -2,
   0,  45,  47,   0,   0, 366, 363,   0,   0, 351,
 344, 334, 335, 336, 337, 338, 339, 340,   0,   0,
 379, 320,  12, 389,  48,   0,  46,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0, 364, 355, 365,
 359,   0,   0,   0,   0,   0,   0,   0, 320, 343,
   0,   0,   0, 342,   0,   0, 321, 237, 235,  47,
  39, 388, 383, 382, 367, 368, 369, 370, 371, 372,
 373, 374, 352, 353,   0,   0,   0,   0,   0,   0,
 345, 346, 347, 348, 354, 349, 350,   0,   0,   0,
 314, 321, 379, 380, 328,   0, 330,  -2, 333, 341,
 316, 317,   0,   0, 389,  73,   8, 386, 381, 356,
 357, 358, 360, 361, 362, 311,   0, 318, 319,   0,
   0, 329,   0, 238, 236,  13,   0,  40,  75,  36,
   0,   0,   0, 384, 385, 387, 315,   0, 313, 312,
 331,   0, 383,  37,   0,  74, 122, 377, 306, 213,
  71,  72,  73,   0,   0,   0, 389, 277,   0,   9,
  10,  11,  39,   0,   5,  38,   0,   0,  76,  -2,
 121, 126, 127, 136, 137, 138, 139, 140, 141, 142,
 143, 144, 145, 146, 148, 129,   0,   0,   0,  97,
  90,   0,   0, 131, 203, 204,  68,  69,  70,   0,
  86,  45,  83,  62,   0,  77,   0,   0,   0,   0,
  33, 279, 280,   0,  73,   0,   0,   0,  -2, 119,
   0,  -2, 128, 147, 181, 178,   0,   0, 166, 159,
 151, 152, 153, 154, 155, 156,   0,   0, 326, 203,
   0,  92,  93,   0,   0,  99, 100,   0,   0,   0,
 122, 122,   0,   0, 132, 133, 134, 135,   0, 198,
 201,   0,   0, 199,   0, 202, 378,   0,   0, 307,
   0,   0, 214,   0, 277,   0,   0,   0,   0,   0,
  -2,   0,   0,  60, 389,  64,   0,   0,   0,  59,
  78,  79,  80,  81,  82, 278, 389, 389,   0,  47,
   6,  45,  47,   0, 123, 125, 118,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0, 179, 170, 180,
 174,   0,   0,   0,   0,   0,   0,   0,   0, 158,
   0,   0, 327, 241, 239, 326, 130,  94, 122, 111,
 112, 103, 122,  87,  88, 122,   0,   0,   0, 122,
   0, 192, 190, 149,   0, 194,   0, 200,   0, 376,
   0, 301, 303, 305,   0,   0,   0,   0,   0,   0,
   0, 212,  85,  47,   0,  84,  63,  61,   0,  66,
   0,   0,   0,  34,  35, 383, 389,  16, 389,   0,
 182, 183, 184, 185, 186, 187, 188, 189, 167, 168,
   0,   0,   0,   0,   0,   0, 160, 161, 162, 163,
 169, 164, 165, 157, 322, 323,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,  91,   0,   0,
   0, 196,   0, 150,   0, 197,   0,   0,   0, 291,
 292, 293, 294, 295, 296, 297,   0, 288, 289, 290,
 263,   0, 224, 249,   0, 224, 221,   0,   0, 264,
   0, 308,   0,   0,   0,   0,   0,   0,   0, 389,
   0,  65,   0,  56,  53,   0, 263,   0,   0,  14,
  39,  47,  39, 383, 171, 172, 173, 175, 176, 177,
 242, 240, 324, 325, 113, 114, 122,   0, 106, 109,
 122, 122, 122, 101, 102,  98,   0,  89, 193, 191,
 195, 375, 300,   0,   0,   0,   0,   0,   0,   0,
 224, 225, 226, 227, 228, 229,   0, 249, 249, 258,
   0,   0,   0,   0,   0,   0, 215,   0, 283, 281,
   0,   0, 389,   0,   0, 308, 308,   0, 308, 308,
   0,  39, 389,  67,   0,   0,  57,  58,   0,  44,
 389,  44,  25,   0,   0,   0,   0,   0, 107, 108,
   0, 298, 304, 299,   0, 268,   0,   0, 269, 271,
   0,   0,   0,   0, 249, 260,   0,   0,   0,   0,
   0, 379, 246,   0,   0, 232,   0,   0,   0,   0,
 216, 218,   0,   0, 285, 286, 287,   0,   0, 302,
 205, 320, 310, 389, 389, 308, 389, 389, 308,  44,
  52,  54,   0,  15,  31,   0,  39,  31,   0, 115,
 122, 122, 122, 110, 122,   0,   0, 273,   0, 270,
 272, 265,   0,   0, 259, 261, 262, 250, 243,   0,
   0,   0,   0, 233, 234,  -2, 231,   0, 222, 219,
 220, 284, 282,   0,   0, 206, 207, 389, 208, 209,
 389,  73,  55,   0, 389,   0, 389,  73,   0,  26,
   0, 104, 105,   0,   0,   0, 274, 266, 267, 256,
 254,   0,   0, 243,   0, 247, 248, 217,   0, 275,
 309, 210, 211,   0, 383,  32, 377, 306, 213,  30,
  41, 389,  43,   0, 383, 116,  95, 122, 276,   0,
 243, 249, 249, 249, 256, 243, 223, 389,  19,  27,
  28,  29,  42, 383,  22,   0, 251, 255, 257, 244,
 245,   0, 256,  50,   0,  17,   0,  96, 252,   0,
  20,   0,  23, 253,  18 };
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
# line 211 "mod2.gram"
{ printf("Fatal error, cannot proceed\n");
				    exit(1); } break;
case 4:
# line 219 "mod2.gram"
{ yyval = DefineModule(yypvt[-1],MODULE); } break;
case 5:
# line 221 "mod2.gram"
{ EndModule(yypvt[-6],yypvt[-2],yypvt[-1]); } break;
case 7:
# line 222 "mod2.gram"
{ yyval = DefineModule(yypvt[-1],MODULE); } break;
case 8:
# line 224 "mod2.gram"
{ EndModule(yypvt[-3],0,yypvt[-1]); } break;
case 10:
# line 228 "mod2.gram"
{ EndFile(); } break;
case 11:
# line 230 "mod2.gram"
{ yyerror("Junk after end of module");
		  EndFile(); 
		  yychar = ENDOFFILE; } break;
case 12:
# line 237 "mod2.gram"
{ yyval = DefineModule(yypvt[-1],IMPLEMENTATION); } break;
case 13:
# line 238 "mod2.gram"
{ GetDefinitionModule(yypvt[-2]);} break;
case 14:
# line 241 "mod2.gram"
{ EndModule(yypvt[-8],yypvt[-2],yypvt[-1]); } break;
case 16:
# line 246 "mod2.gram"
{ yyval = ContinueModule(yypvt[-1]); } break;
case 17:
# line 249 "mod2.gram"
{ DisContinueModule(yypvt[-6],yypvt[-2],yypvt[-1]); } break;
case 19:
# line 255 "mod2.gram"
{ EndImplDef(yypvt[-1]); } break;
case 21:
# line 260 "mod2.gram"
{ yyval = DefineModule(yypvt[-0],DEFINITION); } break;
case 22:
# line 262 "mod2.gram"
{ EndModule(yypvt[-7],0,yypvt[-1]); } break;
case 23:
# line 263 "mod2.gram"
{ GetInlineImpl(yypvt[-9]) ; } break;
case 24:
# line 264 "mod2.gram"
{ yyval = DefineModule(yypvt[-0],DEFINITION); } break;
case 25:
# line 265 "mod2.gram"
{ EndModule(yypvt[-3],0,yypvt[-1]); } break;
case 26:
# line 266 "mod2.gram"
{ GetInlineImpl(yypvt[-5]); } break;
case 30:
# line 277 "mod2.gram"
{ EndProc(yypvt[-0],0,0); } break;
case 33:
# line 289 "mod2.gram"
{ yyval = ImportDecl(0,yypvt[-1]); } break;
case 34:
# line 291 "mod2.gram"
{ yyval = ImportDecl(yypvt[-3],yypvt[-1]); } break;
case 35:
# line 293 "mod2.gram"
{ yyval = ImportDecl(yypvt[-3],0); } break;
case 36:
# line 298 "mod2.gram"
{ yyval = ReadImport(yypvt[-0]); } break;
case 37:
# line 300 "mod2.gram"
{ yyval = ReadImport(yypvt[-1]); } break;
case 38:
# line 303 "mod2.gram"
{ yyval = ReadImport(yypvt[-2]); } break;
case 40:
# line 311 "mod2.gram"
{ ProcessImport(yypvt[-0],0); } break;
case 41:
# line 316 "mod2.gram"
{ ExportDecl(yypvt[-1],EXPORT); } break;
case 42:
# line 318 "mod2.gram"
{ ExportDecl(yypvt[-1],QUALIFIED); } break;
case 43:
# line 320 "mod2.gram"
{ ExportAll(); } break;
case 44:
# line 322 "mod2.gram"
{ ExportMissing(); } break;
case 46:
# line 329 "mod2.gram"
{ yyval = yypvt[-1]; } break;
case 48:
# line 336 "mod2.gram"
{ SetNoInit(); } break;
case 49:
# line 340 "mod2.gram"
{ yyval = DefineModule(yypvt[-1],MODULE); } break;
case 50:
# line 343 "mod2.gram"
{ EndModule(yypvt[-7],yypvt[-2],yypvt[-1]); } break;
case 51:
# line 344 "mod2.gram"
{ yyval = DefineModule(yypvt[-1],MODULE); } break;
case 52:
# line 346 "mod2.gram"
{ EndModule(yypvt[-3],0,yypvt[-1]); } break;
case 53:
# line 351 "mod2.gram"
{ yyval = TypeOf(yypvt[-0]); } break;
case 54:
# line 353 "mod2.gram"
{ yyval = ArrayType(0,yypvt[-0],yypvt[-2],0); } break;
case 55:
# line 355 "mod2.gram"
{ yyval = ArrayType(0,yypvt[-0],yypvt[-3],ATNOCOUNT); } break;
case 56:
# line 360 "mod2.gram"
{ yyval = MakeParamList(0,yypvt[-2],yypvt[-0]); } break;
case 57:
# line 362 "mod2.gram"
{ yyval = MakeParamList(VAR,yypvt[-2],yypvt[-0]); } break;
case 58:
# line 364 "mod2.gram"
{ yyval = MakeParamList(CONST,yypvt[-2],yypvt[-0]); } break;
case 59:
# line 367 "mod2.gram"
{ yyval = MakeParamList(0,0,anyTypeNode); } break;
case 60:
# line 372 "mod2.gram"
{ yyval = 0; } break;
case 61:
# line 374 "mod2.gram"
{ yyval = yypvt[-1]; } break;
case 62:
# line 379 "mod2.gram"
{ yyval = 0; } break;
case 63:
# line 381 "mod2.gram"
{ yyval = TypeOf(yypvt[-0]); } break;
case 65:
# line 389 "mod2.gram"
{ yyval = AppendParamList(yypvt[-2],yypvt[-0]); } break;
case 66:
# line 391 "mod2.gram"
{ yyerror("Must use ';' rather than ','"); } break;
case 67:
# line 393 "mod2.gram"
{ yyval = AppendParamList(yypvt[-3],yypvt[-0]); } break;
case 75:
# line 418 "mod2.gram"
{ yyval = AddToStmtList(0,0); } break;
case 76:
# line 420 "mod2.gram"
{ yyval = yypvt[-0]; } break;
case 77:
# line 426 "mod2.gram"
{ yyval = DefineProc(yypvt[-0],PROCEDURE); } break;
case 78:
# line 428 "mod2.gram"
{ temp = DefineProc(yypvt[-1],PROCEDURE);
                                    SetGlobalProcName(temp,yypvt[-0]); yyval = temp;} break;
case 79:
# line 431 "mod2.gram"
{ yyval = DefineProc(yypvt[-0],ATEXTERNAL); } break;
case 80:
# line 433 "mod2.gram"
{ yyval = DefineProc(yypvt[-0],ATGLOBAL); } break;
case 81:
# line 435 "mod2.gram"
{ yyval = DefineProc(yypvt[-0],ATINLINE); } break;
case 82:
# line 437 "mod2.gram"
{ yyval = DefineProc(yypvt[-0],ATASM); } break;
case 83:
# line 442 "mod2.gram"
{ yyval = AddTypeToProc(yypvt[-1],procTypeNode); } break;
case 84:
# line 444 "mod2.gram"
{ temp = ProcType(yypvt[-2],yypvt[-1]);
				    yyval = AddTypeToProc(yypvt[-3],temp); } break;
case 85:
# line 450 "mod2.gram"
{ EndProc(yypvt[-3],yypvt[-2],yypvt[-1]); } break;
case 86:
# line 452 "mod2.gram"
{ EndProc(yypvt[-1],0,0); } break;
case 88:
# line 459 "mod2.gram"
{ yyerror("Use DO instead of THEN"); } break;
case 89:
# line 464 "mod2.gram"
{ yyval = BuildStmtWith(yypvt[-3],yypvt[-1]); } break;
case 90:
# line 468 "mod2.gram"
{ yyval = StartStmtLoop(); } break;
case 91:
# line 469 "mod2.gram"
{ yyval = BuildStmtLoop(yypvt[-2],yypvt[-1]); } break;
case 93:
# line 476 "mod2.gram"
{ yyerror("Use ':=' instead of '=' for assignment"); } break;
case 94:
# line 478 "mod2.gram"
{ yyerror("':=' cannot have imbedded spaces"); } break;
case 95:
# line 484 "mod2.gram"
{ yyval = BuildStmtFor(yypvt[-7],yypvt[-5],yypvt[-3],0,yypvt[-1]); } break;
case 96:
# line 487 "mod2.gram"
{ yyval = BuildStmtFor(yypvt[-9],yypvt[-7],yypvt[-5],yypvt[-3],yypvt[-1]); } break;
case 97:
# line 491 "mod2.gram"
{ yyval = StartStmtRepeat(); } break;
case 98:
# line 493 "mod2.gram"
{ yyval = BuildStmtRepeat(yypvt[-3],yypvt[-2],yypvt[-0]); } break;
case 100:
# line 500 "mod2.gram"
{ yyval = BuildExprBad(); } break;
case 101:
# line 505 "mod2.gram"
{ yyval = BuildStmtWhile(yypvt[-3],yypvt[-1]); } break;
case 102:
# line 507 "mod2.gram"
{ temp = BuildExprBad();
				  yyval = BuildStmtWhile(temp,yypvt[-1]); } break;
case 103:
# line 513 "mod2.gram"
{ yyval = 0; } break;
case 104:
# line 515 "mod2.gram"
{ yyval = AddCase(yypvt[-4],yypvt[-2],yypvt[-0]); } break;
case 105:
# line 517 "mod2.gram"
{ yyval = AddCase(yypvt[-4],0,yypvt[-0]); } break;
case 106:
# line 519 "mod2.gram"
{ yyval = yypvt[-1]; } break;
case 107:
# line 521 "mod2.gram"
{ yyval = AddCase(0,yypvt[-2],yypvt[-0]); } break;
case 108:
# line 523 "mod2.gram"
{ yyval = AddCase(0,0,yypvt[-0]); } break;
case 109:
# line 528 "mod2.gram"
{ yyval = BuildStmtCase(yypvt[-3],yypvt[-1],0); } break;
case 110:
# line 530 "mod2.gram"
{ yyval = BuildStmtCase(yypvt[-5],yypvt[-3],yypvt[-1]); } break;
case 112:
# line 537 "mod2.gram"
{ yyerror("Use THEN instead of DO"); } break;
case 113:
# line 542 "mod2.gram"
{ yyval = BuildStmtIf(yypvt[-3],yypvt[-1],yypvt[-0]); } break;
case 114:
# line 547 "mod2.gram"
{ yyval = AddToStmtList(0,0); } break;
case 115:
# line 549 "mod2.gram"
{ yyval = yypvt[-1]; } break;
case 116:
# line 551 "mod2.gram"
{ temp = BuildStmtIf(yypvt[-3],yypvt[-1],yypvt[-0]);
				    yyval = AddToStmtList(0,temp); } break;
case 118:
# line 560 "mod2.gram"
{ yyval = yypvt[-1]; } break;
case 120:
# line 564 "mod2.gram"
{ yyval = yypvt[-0]; } break;
case 121:
# line 566 "mod2.gram"
{ yyval = AddToStmtList(0,0); } break;
case 122:
# line 568 "mod2.gram"
{ yyval = AddToStmtList(0,0); } break;
case 123:
# line 573 "mod2.gram"
{ yyval = AddToStmtList(yypvt[-2],yypvt[-0]); } break;
case 124:
# line 574 "mod2.gram"
{ yyerror("Missing semicolon"); } break;
case 125:
# line 575 "mod2.gram"
{ yyval = AddToStmtList(yypvt[-2],yypvt[-0]); } break;
case 126:
# line 577 "mod2.gram"
{ yyval = AddToStmtList(0,yypvt[-0]); } break;
case 129:
# line 587 "mod2.gram"
{ temp = AddToExprList(0,0);
				  yyval = BuildStmtProc(yypvt[-0],0); } break;
case 130:
# line 593 "mod2.gram"
{ yyval = BuildStmtAssign(yypvt[-2],yypvt[-0]); } break;
case 131:
# line 602 "mod2.gram"
{ yyval = BuildStmtExit(LOOP); } break;
case 132:
# line 604 "mod2.gram"
{ yyval = BuildStmtExit(LOOP); } break;
case 133:
# line 606 "mod2.gram"
{ yyval = BuildStmtExit(FOR); } break;
case 134:
# line 608 "mod2.gram"
{ yyval = BuildStmtExit(WHILE); } break;
case 135:
# line 610 "mod2.gram"
{ yyval = BuildStmtExit(REPEAT); } break;
case 146:
# line 638 "mod2.gram"
{ yyval = BuildStmtReturn(0); } break;
case 147:
# line 640 "mod2.gram"
{ yyval = BuildStmtReturn(yypvt[-0]); } break;
case 148:
# line 642 "mod2.gram"
{ yyval = 0; } break;
case 149:
# line 647 "mod2.gram"
{ yyval = AddToExprList(0,0); } break;
case 150:
# line 649 "mod2.gram"
{ yyval = yypvt[-1]; } break;
case 151:
# line 654 "mod2.gram"
{ yyval = BuildExprConst(yypvt[-0]); } break;
case 152:
# line 656 "mod2.gram"
{ yyval = BuildExprConst(yypvt[-0]); } break;
case 153:
# line 658 "mod2.gram"
{ yyval = BuildExprConst(yypvt[-0]); } break;
case 154:
# line 660 "mod2.gram"
{ yyval = BuildExprConst(yypvt[-0]); } break;
case 157:
# line 666 "mod2.gram"
{ yyval = yypvt[-1]; } break;
case 158:
# line 668 "mod2.gram"
{ yyval = BuildExprUnOp(NOT,yypvt[-0]); } break;
case 160:
# line 675 "mod2.gram"
{ yyval = BuildExprBinOp(ASTERISK,yypvt[-2],yypvt[-0]); } break;
case 161:
# line 677 "mod2.gram"
{ yyval = BuildExprBinOp(SLASH,yypvt[-2],yypvt[-0]); } break;
case 162:
# line 679 "mod2.gram"
{ yyval = BuildExprBinOp(DIV,yypvt[-2],yypvt[-0]); } break;
case 163:
# line 681 "mod2.gram"
{ yyval = BuildExprBinOp(MOD,yypvt[-2],yypvt[-0]); } break;
case 164:
# line 683 "mod2.gram"
{ yyval = BuildExprBinOp(AND,yypvt[-2],yypvt[-0]); } break;
case 165:
# line 685 "mod2.gram"
{ yyval = BuildExprBinOp(AMPERSAND,yypvt[-2],yypvt[-0]); } break;
case 167:
# line 692 "mod2.gram"
{ yyval = BuildExprBinOp(PLUS,yypvt[-2],yypvt[-0]); } break;
case 168:
# line 694 "mod2.gram"
{ yyval = BuildExprBinOp(MINUS,yypvt[-2],yypvt[-0]); } break;
case 169:
# line 696 "mod2.gram"
{ yyval = BuildExprBinOp(OR,yypvt[-2],yypvt[-0]); } break;
case 170:
# line 701 "mod2.gram"
{ yyval = BuildExprUnOp(PLUS,yypvt[-0]); } break;
case 171:
# line 703 "mod2.gram"
{ yyval = BuildExprBinOp(PLUS,yypvt[-2],yypvt[-0]); } break;
case 172:
# line 705 "mod2.gram"
{ yyval = BuildExprBinOp(MINUS,yypvt[-2],yypvt[-0]); } break;
case 173:
# line 707 "mod2.gram"
{ temp = BuildExprUnOp(PLUS,yypvt[-2]);
				  yyval = BuildExprBinOp(OR,temp,yypvt[-0]); } break;
case 174:
# line 713 "mod2.gram"
{ yyval = BuildExprUnOp(MINUS,yypvt[-0]); } break;
case 175:
# line 715 "mod2.gram"
{ yyval = BuildExprBinOp(PLUS,yypvt[-2],yypvt[-0]); } break;
case 176:
# line 717 "mod2.gram"
{ yyval = BuildExprBinOp(MINUS,yypvt[-2],yypvt[-0]); } break;
case 177:
# line 719 "mod2.gram"
{ temp = BuildExprUnOp(MINUS,yypvt[-2]);
				  yyval = BuildExprBinOp(OR,temp,yypvt[-0]); } break;
case 179:
# line 727 "mod2.gram"
{ yyval = yypvt[-0]; } break;
case 180:
# line 729 "mod2.gram"
{ yyval = yypvt[-0]; } break;
case 182:
# line 736 "mod2.gram"
{ yyval = BuildExprBinOp(EQUALS,yypvt[-2],yypvt[-0]); } break;
case 183:
# line 738 "mod2.gram"
{ yyval = BuildExprBinOp(SHARP,yypvt[-2],yypvt[-0]); } break;
case 184:
# line 740 "mod2.gram"
{ yyval = BuildExprBinOp(NOTEQUAL,yypvt[-2],yypvt[-0]); } break;
case 185:
# line 742 "mod2.gram"
{ yyval = BuildExprBinOp(LESS,yypvt[-2],yypvt[-0]); } break;
case 186:
# line 744 "mod2.gram"
{ yyval = BuildExprBinOp(LSEQUAL,yypvt[-2],yypvt[-0]); } break;
case 187:
# line 746 "mod2.gram"
{ yyval = BuildExprBinOp(GREATER,yypvt[-2],yypvt[-0]); } break;
case 188:
# line 748 "mod2.gram"
{ yyval = BuildExprBinOp(GREQUAL,yypvt[-2],yypvt[-0]); } break;
case 189:
# line 750 "mod2.gram"
{ yyval = BuildExprBinOp(IN,yypvt[-2],yypvt[-0]); } break;
case 191:
# line 757 "mod2.gram"
{ yyval = BuildExprRange(yypvt[-2],yypvt[-0]); } break;
case 192:
# line 762 "mod2.gram"
{ yyval = AddToExprList(0,yypvt[-0]); } break;
case 193:
# line 764 "mod2.gram"
{ yyval = AddToExprList(yypvt[-2],yypvt[-0]); } break;
case 194:
# line 769 "mod2.gram"
{ yyval = AddToExprList(0,yypvt[-0]); } break;
case 195:
# line 771 "mod2.gram"
{ yyval = AddToExprList(yypvt[-2],yypvt[-0]); } break;
case 196:
# line 777 "mod2.gram"
{ temp = BuildExprSym(yypvt[-3]);
				    yyval = BuildExprSubscript(temp,yypvt[-1]); } break;
case 197:
# line 780 "mod2.gram"
{ yyval = BuildExprSubscript(yypvt[-3],yypvt[-1]); } break;
case 198:
# line 782 "mod2.gram"
{ temp = BuildExprSym(yypvt[-1]);
				    yyval = BuildExprDeref(temp); } break;
case 199:
# line 785 "mod2.gram"
{ yyval = BuildExprDeref(yypvt[-1]); } break;
case 200:
# line 787 "mod2.gram"
{ yyval = BuildExprDot(yypvt[-2],yypvt[-0]); } break;
case 201:
# line 789 "mod2.gram"
{ temp = BuildExprSym(yypvt[-1]);
				  yyval = BuildExprFunc(temp,yypvt[-0]); } break;
case 202:
# line 792 "mod2.gram"
{ yyval = BuildExprFunc(yypvt[-1],yypvt[-0]); } break;
case 203:
# line 797 "mod2.gram"
{ yyval = BuildExprSym(yypvt[-0]); } break;
case 205:
# line 806 "mod2.gram"
{ DefineVarList(yypvt[-4],yypvt[-2],VAR,0,yypvt[-1],0); } break;
case 206:
# line 808 "mod2.gram"
{ temp = MakeIdent(yypvt[-5]);
                                    temp = AddToIdentList(0,temp);
                                    DefineVarList(temp,yypvt[-2],VAR,0,yypvt[-1],yypvt[-4]); } break;
case 207:
# line 812 "mod2.gram"
{ DefineVarList(yypvt[-4],yypvt[-2],VAR,ATSHARED,yypvt[-1],0); } break;
case 208:
# line 814 "mod2.gram"
{ DefineVarList(yypvt[-4],yypvt[-2],ATEXTERNAL,0,yypvt[-1],0); } break;
case 209:
# line 816 "mod2.gram"
{ DefineVarList(yypvt[-4],yypvt[-2],ATGLOBAL,0,yypvt[-1],0); } break;
case 210:
# line 818 "mod2.gram"
{ DefineVarList(yypvt[-4],yypvt[-2],ATGLOBAL,ATSHARED,yypvt[-1],0);} break;
case 211:
# line 820 "mod2.gram"
{ DefineVarList(yypvt[-4],yypvt[-2],ATGLOBAL,ATSHARED,yypvt[-1],0);} break;
case 215:
# line 835 "mod2.gram"
{ yyval = 0; } break;
case 217:
# line 839 "mod2.gram"
{ yyval = AppendParamList(yypvt[-2],yypvt[-0]); } break;
case 218:
# line 844 "mod2.gram"
{ yyval = MakeParamList(0,0,yypvt[-0]); } break;
case 219:
# line 846 "mod2.gram"
{ yyval = MakeParamList(VAR,0,yypvt[-0]); } break;
case 220:
# line 848 "mod2.gram"
{ yyval = MakeParamList(CONST,0,yypvt[-0]); } break;
case 221:
# line 853 "mod2.gram"
{ yyval = procTypeNode; } break;
case 222:
# line 855 "mod2.gram"
{ yyval = ProcType(yypvt[-1],0); } break;
case 223:
# line 857 "mod2.gram"
{ temp = TypeOf(yypvt[-0]); yyval = ProcType(yypvt[-3],temp); } break;
case 224:
# line 861 "mod2.gram"
{ yyval = POINTER; } break;
case 225:
# line 863 "mod2.gram"
{ yyval = ATPASCAL; } break;
case 226:
# line 865 "mod2.gram"
{ yyval = ATC; } break;
case 227:
# line 867 "mod2.gram"
{ yyval = ATNOCHECK; } break;
case 228:
# line 869 "mod2.gram"
{ yyval = ATNILCHECK; } break;
case 229:
# line 871 "mod2.gram"
{ yyval = ATLOCAL; } break;
case 230:
# line 881 "mod2.gram"
{ yyval = PointerForwardType(yypvt[-0],yypvt[-2]); } break;
case 231:
# line 883 "mod2.gram"
{ yyval = PointerType(yypvt[-0],yypvt[-2]); } break;
case 232:
# line 888 "mod2.gram"
{ yyval = SetType(yypvt[-0],0); } break;
case 233:
# line 890 "mod2.gram"
{ yyval = SetType(yypvt[-0],ATLEFTTORIGHT); } break;
case 234:
# line 892 "mod2.gram"
{ yyval = SetType(yypvt[-0],ATRIGHTTOLEFT); } break;
case 235:
# line 897 "mod2.gram"
{ yyval = MakeConstSet(yypvt[-0],0); } break;
case 236:
# line 899 "mod2.gram"
{ yyval = MakeConstSet(yypvt[-2],yypvt[-0]); } break;
case 237:
# line 904 "mod2.gram"
{ yyval = AddToConstSetList(0,yypvt[-0]); } break;
case 238:
# line 906 "mod2.gram"
{ yyval = AddToConstSetList(yypvt[-2],yypvt[-0]); } break;
case 239:
# line 911 "mod2.gram"
{ yyval = MakeExprSet(yypvt[-0],0); } break;
case 240:
# line 913 "mod2.gram"
{ yyval = MakeExprSet(yypvt[-2],yypvt[-0]); } break;
case 241:
# line 918 "mod2.gram"
{ yyval = AddToExprSetList(0,yypvt[-0]); } break;
case 242:
# line 920 "mod2.gram"
{ yyval = AddToExprSetList(yypvt[-2],yypvt[-0]); } break;
case 243:
# line 925 "mod2.gram"
{ yyval = 0; } break;
case 244:
# line 927 "mod2.gram"
{ yyval = MakeVariant(yypvt[-2],yypvt[-0]); } break;
case 245:
# line 929 "mod2.gram"
{ yyval = MakeVariant(0,yypvt[-0]); } break;
case 246:
# line 934 "mod2.gram"
{ yyval = TypeOf(yypvt[-0]); } break;
case 247:
# line 936 "mod2.gram"
{ yyval = TypeWithSize(yypvt[-0],yypvt[-1]); } break;
case 248:
# line 938 "mod2.gram"
{ yyval = TypeWithAlign(yypvt[-0],yypvt[-1]); } break;
case 249:
# line 943 "mod2.gram"
{ yyval = EmptyFieldList(); } break;
case 250:
# line 945 "mod2.gram"
{ yyval = MakeFieldList(yypvt[-2],yypvt[-0]); } break;
case 251:
# line 948 "mod2.gram"
{ yyval = MakeTagField(0,yypvt[-4],yypvt[-2],yypvt[-1]); } break;
case 252:
# line 950 "mod2.gram"
{ yyval = MakeTagField(0,yypvt[-4],yypvt[-2],yypvt[-1]); } break;
case 253:
# line 952 "mod2.gram"
{ yyval = MakeTagField(yypvt[-6],yypvt[-4],yypvt[-2],yypvt[-1]); } break;
case 254:
# line 957 "mod2.gram"
{ yyval = AddToVariantList(0,yypvt[-0]); } break;
case 255:
# line 959 "mod2.gram"
{ yyval = AddToVariantList(yypvt[-2],yypvt[-0]); } break;
case 256:
# line 964 "mod2.gram"
{ temp = EmptyFieldList();
				  yyval = MakeVariant(0,temp); } break;
case 257:
# line 967 "mod2.gram"
{ yyval = MakeVariant(0,yypvt[-0]); } break;
case 259:
# line 974 "mod2.gram"
{ yyval = AppendFieldList(yypvt[-2],yypvt[-0]); } break;
case 260:
# line 979 "mod2.gram"
{ yyval = RecordType(yypvt[-1],0); } break;
case 261:
# line 981 "mod2.gram"
{ yyval = RecordType(yypvt[-1],ATLEFTTORIGHT); } break;
case 262:
# line 983 "mod2.gram"
{ yyval = RecordType(yypvt[-1],ATRIGHTTOLEFT); } break;
case 263:
# line 988 "mod2.gram"
{ yyval = ARRAY; } break;
case 264:
# line 990 "mod2.gram"
{ yyval = ATSUBARRAY; } break;
case 265:
# line 995 "mod2.gram"
{ yyval = DynArrayType(yypvt[-0],0,yypvt[-2]); } break;
case 266:
# line 997 "mod2.gram"
{ yyval = DynArrayType(yypvt[-0],ATNOCOUNT,yypvt[-3]); } break;
case 267:
# line 999 "mod2.gram"
{ yyval = DynArrayType(yypvt[-0],ATNOCOUNT,yypvt[-2]); } break;
case 268:
# line 1005 "mod2.gram"
{ yyval = ArrayType(yypvt[-1],yypvt[-0],ARRAY,0); } break;
case 269:
# line 1007 "mod2.gram"
{ temp = TypeOf(yypvt[-0]);
				    yyval = ArrayType(0,temp,yypvt[-2],0); } break;
case 270:
# line 1010 "mod2.gram"
{ temp = TypeOf(yypvt[-0]);
				    yyval = ArrayType(0,temp,yypvt[-3],ATNOCOUNT); } break;
case 271:
# line 1013 "mod2.gram"
{ yyval = ArrayType(0,yypvt[-0],yypvt[-2],0); } break;
case 272:
# line 1015 "mod2.gram"
{ yyval = ArrayType(0,yypvt[-0],yypvt[-3],ATNOCOUNT); } break;
case 273:
# line 1020 "mod2.gram"
{ yyval = yypvt[-0]; } break;
case 274:
# line 1023 "mod2.gram"
{ yyval = ArrayType(yypvt[-1],yypvt[-0],ARRAY,0); } break;
case 275:
# line 1028 "mod2.gram"
{ yyval = SubrangeType(yypvt[-3],yypvt[-1],0); } break;
case 276:
# line 1031 "mod2.gram"
{ temp = TypeOf(yypvt[-5]);
				    yyval = SubrangeType(yypvt[-3],yypvt[-1],temp); } break;
case 277:
# line 1037 "mod2.gram"
{ temp = MakeIdent(yypvt[-0]);
				    yyval = AddToIdentList(0,temp); } break;
case 278:
# line 1041 "mod2.gram"
{ temp = MakeIdent(yypvt[-0]);
				    yyval = AddToIdentList(yypvt[-2],temp); } break;
case 279:
# line 1044 "mod2.gram"
{ yyval = yypvt[-1];
				  yyerror("Identifier expected"); } break;
case 280:
# line 1047 "mod2.gram"
{ temp = MakeIdent(yypvt[-0]);
				    yyval = AddToIdentList(yypvt[-1],temp);
				    yyerror("Comma expected"); } break;
case 281:
# line 1054 "mod2.gram"
{ yyval = MakeEnumNode(yypvt[-0],0); } break;
case 282:
# line 1056 "mod2.gram"
{ yyval = MakeEnumNode(yypvt[-2],yypvt[-0]); } break;
case 283:
# line 1062 "mod2.gram"
{ yyval = AddToEnumList(0,yypvt[-0]); } break;
case 284:
# line 1065 "mod2.gram"
{ yyval = AddToEnumList(yypvt[-2],yypvt[-0]); } break;
case 285:
# line 1067 "mod2.gram"
{ yyval = yypvt[-1];
				  yyerror("Identifier expected"); } break;
case 286:
# line 1070 "mod2.gram"
{ yyval = AddToEnumList(yypvt[-1],yypvt[-0]);
				  yyerror("Comma expected"); } break;
case 287:
# line 1076 "mod2.gram"
{ yyval =  EnumerationType(yypvt[-1]); } break;
case 288:
# line 1081 "mod2.gram"
{ yyval = TypeOf(yypvt[-0]); } break;
case 298:
# line 1106 "mod2.gram"
{ yyval = TypeWithSize(yypvt[-0],yypvt[-1]); } break;
case 299:
# line 1108 "mod2.gram"
{ yyval = TypeWithAlign(yypvt[-0],yypvt[-1]); } break;
case 300:
# line 1113 "mod2.gram"
{ DefineType(yypvt[-3],yypvt[-1]); } break;
case 301:
# line 1115 "mod2.gram"
{ yyerror("Use '=' instead of ':' for type declarations"); } break;
case 302:
# line 1117 "mod2.gram"
{DefineType(yypvt[-4],yypvt[-1]); } break;
case 303:
# line 1119 "mod2.gram"
{ DefineType(yypvt[-1],0); } break;
case 304:
# line 1121 "mod2.gram"
{ temp = OpaqueWithSize(yypvt[-4],yypvt[-1]);
				    DefineType(yypvt[-4],temp); } break;
case 308:
# line 1137 "mod2.gram"
{ yyval = 0; } break;
case 309:
# line 1139 "mod2.gram"
{ yyval = yypvt[-1]; } break;
case 310:
# line 1141 "mod2.gram"
{ temp = MakeFormerConstElement(0,yypvt[-0]);
                                    yyval = AddToFormerElementList(0,temp); } break;
case 311:
# line 1148 "mod2.gram"
{ yyval = ConstFormer(yypvt[-3],yypvt[-1]); } break;
case 312:
# line 1157 "mod2.gram"
{ yyval = MakeFormerFieldElement(yypvt[-2],yypvt[-0]); } break;
case 313:
# line 1159 "mod2.gram"
{ yyval = MakeFormerConstElement(yypvt[-2],yypvt[-0]); } break;
case 314:
# line 1164 "mod2.gram"
{ yyval = AddToFormerElementList(0,yypvt[-0]); } break;
case 315:
# line 1166 "mod2.gram"
{ yyval = AddToFormerElementList(yypvt[-2],yypvt[-0]); } break;
case 316:
# line 1172 "mod2.gram"
{ yyval = ConstSet(yypvt[-1],0); } break;
case 317:
# line 1174 "mod2.gram"
{ yyval = ConstSet(0,0); } break;
case 318:
# line 1176 "mod2.gram"
{ temp = TypeOf(yypvt[-3]); yyval = ConstSet(yypvt[-1],temp); } break;
case 319:
# line 1178 "mod2.gram"
{ temp = TypeOf(yypvt[-3]); yyval = ConstSet(0,temp); } break;
case 320:
# line 1183 "mod2.gram"
{ yyval = 0; } break;
case 322:
# line 1190 "mod2.gram"
{ yyval = BuildExprSet(yypvt[-1],0); } break;
case 323:
# line 1192 "mod2.gram"
{ yyval = BuildExprSet(0,0); } break;
case 324:
# line 1194 "mod2.gram"
{ yyval = BuildExprSet(yypvt[-1],yypvt[-3]); } break;
case 325:
# line 1196 "mod2.gram"
{ yyval = BuildExprSet(0,yypvt[-3]); } break;
case 326:
# line 1201 "mod2.gram"
{ yyval = 0; } break;
case 328:
# line 1208 "mod2.gram"
{ yyval = 0; } break;
case 329:
# line 1210 "mod2.gram"
{ yyval = yypvt[-1]; } break;
case 330:
# line 1215 "mod2.gram"
{ yyval = AddToConstParamList(0,yypvt[-0]); } break;
case 331:
# line 1217 "mod2.gram"
{ yyval = AddToConstParamList(yypvt[-2],yypvt[-0]); } break;
case 332:
# line 1226 "mod2.gram"
{ yyval = ConstParamIdent(yypvt[-0]); } break;
case 333:
# line 1228 "mod2.gram"
{ yyval = ConstParamConst(yypvt[-0]); } break;
case 334:
# line 1233 "mod2.gram"
{ yyval = ConstSym(yypvt[-0]); } break;
case 341:
# line 1247 "mod2.gram"
{ yyval = yypvt[-1]; } break;
case 342:
# line 1249 "mod2.gram"
{ yyval = ConstUnOp(NOT,yypvt[-0]); } break;
case 343:
# line 1251 "mod2.gram"
{ yyval = ConstBuiltinFunction(yypvt[-1],yypvt[-0]); } break;
case 345:
# line 1259 "mod2.gram"
{ yyval = ConstBinOp(ASTERISK,yypvt[-2],yypvt[-0],0); } break;
case 346:
# line 1261 "mod2.gram"
{ yyval = ConstBinOp(SLASH,yypvt[-2],yypvt[-0],0); } break;
case 347:
# line 1263 "mod2.gram"
{ yyval = ConstBinOp(DIV,yypvt[-2],yypvt[-0],0); } break;
case 348:
# line 1265 "mod2.gram"
{ yyval = ConstBinOp(MOD,yypvt[-2],yypvt[-0],0); } break;
case 349:
# line 1267 "mod2.gram"
{ yyval = ConstBinOp(AND,yypvt[-2],yypvt[-0],0); } break;
case 350:
# line 1269 "mod2.gram"
{ yyval = ConstBinOp(AMPERSAND,yypvt[-2],yypvt[-0], 0); } break;
case 352:
# line 1277 "mod2.gram"
{ yyval = ConstBinOp(PLUS,yypvt[-2],yypvt[-0],0); } break;
case 353:
# line 1279 "mod2.gram"
{ yyval = ConstBinOp(MINUS,yypvt[-2],yypvt[-0],0); } break;
case 354:
# line 1281 "mod2.gram"
{ yyval = ConstBinOp(OR,yypvt[-2],yypvt[-0],0); } break;
case 355:
# line 1286 "mod2.gram"
{ yyval = ConstUnOp(PLUS,yypvt[-0],0); } break;
case 356:
# line 1288 "mod2.gram"
{ yyval = ConstBinOp(PLUS,yypvt[-2],yypvt[-0],0); } break;
case 357:
# line 1290 "mod2.gram"
{ yyval = ConstBinOp(MINUS,yypvt[-2],yypvt[-0],0); } break;
case 358:
# line 1292 "mod2.gram"
{ temp = ConstUnOp(PLUS,yypvt[-2],0);
				  yyval = ConstBinOp(OR,temp,yypvt[-0],0); } break;
case 359:
# line 1298 "mod2.gram"
{ yyval = ConstUnOp(MINUS,yypvt[-0],0); } break;
case 360:
# line 1300 "mod2.gram"
{ yyval = ConstBinOp(PLUS,yypvt[-2],yypvt[-0],0); } break;
case 361:
# line 1302 "mod2.gram"
{ yyval = ConstBinOp(MINUS,yypvt[-2],yypvt[-0],0); } break;
case 362:
# line 1304 "mod2.gram"
{ temp = ConstUnOp(MINUS,yypvt[-2],0);
				  yyval = ConstBinOp(OR,temp,yypvt[-0],0); } break;
case 364:
# line 1312 "mod2.gram"
{ yyval = yypvt[-0]; } break;
case 365:
# line 1314 "mod2.gram"
{ yyval = yypvt[-0]; } break;
case 367:
# line 1322 "mod2.gram"
{ yyval = ConstBinOp(EQUALS,yypvt[-2],yypvt[-0],0); } break;
case 368:
# line 1324 "mod2.gram"
{ yyval = ConstBinOp(SHARP,yypvt[-2],yypvt[-0],0); } break;
case 369:
# line 1326 "mod2.gram"
{ yyval = ConstBinOp(NOTEQUAL,yypvt[-2],yypvt[-0],0); } break;
case 370:
# line 1328 "mod2.gram"
{ yyval = ConstBinOp(LESS,yypvt[-2],yypvt[-0],0); } break;
case 371:
# line 1330 "mod2.gram"
{ yyval = ConstBinOp(LSEQUAL,yypvt[-2],yypvt[-0],0); } break;
case 372:
# line 1332 "mod2.gram"
{ yyval = ConstBinOp(GREATER,yypvt[-2],yypvt[-0],0); } break;
case 373:
# line 1334 "mod2.gram"
{ yyval = ConstBinOp(GREQUAL,yypvt[-2],yypvt[-0],0); } break;
case 374:
# line 1336 "mod2.gram"
{ yyval = ConstBinOp(IN,yypvt[-2],yypvt[-0],0); } break;
case 375:
# line 1341 "mod2.gram"
{ DefineConst(yypvt[-3],yypvt[-1]); } break;
case 379:
# line 1356 "mod2.gram"
{ temp = MakeIdent(yypvt[-0]);
				    yyval = AddToIdentList(0,temp); } break;
case 380:
# line 1359 "mod2.gram"
{ temp = MakeIdent(yypvt[-0]);
				    yyval = AddToIdentList(yypvt[-2],temp); } break;
case 381:
# line 1366 "mod2.gram"
{ yyval = yypvt[-0]; } break;
case 382:
# line 1369 "mod2.gram"
{ yyerror("Missing identifier on procedure/module end"); 
			  yyval = 0; } break;
case 383:
# line 1374 "mod2.gram"
{ ScanEofOK() ; } break;
case 386:
# line 1381 "mod2.gram"
{ yyerror("Global module must end with a period"); } break;
case 387:
# line 1384 "mod2.gram"
{ yyerror("Global module must end with a period"); } break;
case 389:
# line 1391 "mod2.gram"
{ yyerror("Missing semi-colon"); } break; 
		}
		goto yystack;  /* stack new state and value */

	}

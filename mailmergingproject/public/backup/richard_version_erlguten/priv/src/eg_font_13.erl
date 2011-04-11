-module(eg_font_13).
-export([width/1, kern/2, fontName/0, firstChar/0,lastChar/0]).
-export([index/0,ascender/0,capHeight/0,descender/0,italicAngle/0]).
-export([xHeight/0, flags/0, type/0, stemV/0,fontBBox/0,widths/0]).
-export([encoding/0]).
fontName() -> "Times-Bold".
index() -> 13.
type() -> internal.
encoding() -> "AdobeStandardEncoding".
firstChar() ->32.
lastChar() ->255.
ascender() ->676.
capHeight() ->676.
descender() ->-205.
italicAngle() ->0.
xHeight() ->461.
flags() ->32.
stemV() ->0.
fontBBox() ->{-168,-218,1000,935}.
widths() ->[250,333,555,500,500,1000,833,278,333,333,500,570,250,333,250,278,500,500,500,
 500,500,500,500,500,500,500,333,333,570,570,570,500,930,722,667,722,722,667,
 611,778,778,389,500,778,667,944,722,778,611,778,722,556,667,722,722,1000,722,
 722,667,333,278,333,581,500,333,500,556,444,556,444,333,500,556,278,333,556,
 278,833,556,500,556,556,444,389,333,556,500,722,500,500,444,394,220,394,520,
 0,0,0,333,500,500,1000,500,500,333,1000,556,333,1000,0,667,0,0,333,333,500,
 500,350,500,1000,333,1000,389,333,722,0,444,722,0,333,500,500,500,500,220,
 500,333,747,300,500,570,570,747,333,400,570,300,300,333,556,540,250,333,300,
 330,500,750,750,750,500,722,722,722,722,722,722,1000,722,667,667,667,667,389,
 389,389,389,722,722,778,778,778,778,778,570,778,722,722,722,722,722,611,556,
 500,500,500,500,500,500,722,444,444,444,444,444,278,278,278,278,500,556,500,
 500,500,500,500,570,500,556,556,556,556,500,556,500].
width(32)->250;
width(33)->333;
width(34)->555;
width(35)->500;
width(36)->500;
width(37)->1000;
width(38)->833;
width(39)->278;
width(40)->333;
width(41)->333;
width(42)->500;
width(43)->570;
width(44)->250;
width(45)->333;
width(46)->250;
width(47)->278;
width(48)->500;
width(49)->500;
width(50)->500;
width(51)->500;
width(52)->500;
width(53)->500;
width(54)->500;
width(55)->500;
width(56)->500;
width(57)->500;
width(58)->333;
width(59)->333;
width(60)->570;
width(61)->570;
width(62)->570;
width(63)->500;
width(64)->930;
width(65)->722;
width(66)->667;
width(67)->722;
width(68)->722;
width(69)->667;
width(70)->611;
width(71)->778;
width(72)->778;
width(73)->389;
width(74)->500;
width(75)->778;
width(76)->667;
width(77)->944;
width(78)->722;
width(79)->778;
width(80)->611;
width(81)->778;
width(82)->722;
width(83)->556;
width(84)->667;
width(85)->722;
width(86)->722;
width(87)->1000;
width(88)->722;
width(89)->722;
width(90)->667;
width(91)->333;
width(92)->278;
width(93)->333;
width(94)->581;
width(95)->500;
width(96)->333;
width(97)->500;
width(98)->556;
width(99)->444;
width(100)->556;
width(101)->444;
width(102)->333;
width(103)->500;
width(104)->556;
width(105)->278;
width(106)->333;
width(107)->556;
width(108)->278;
width(109)->833;
width(110)->556;
width(111)->500;
width(112)->556;
width(113)->556;
width(114)->444;
width(115)->389;
width(116)->333;
width(117)->556;
width(118)->500;
width(119)->722;
width(120)->500;
width(121)->500;
width(122)->444;
width(123)->394;
width(124)->220;
width(125)->394;
width(126)->520;
width(130)->333;
width(131)->500;
width(132)->500;
width(133)->1000;
width(134)->500;
width(135)->500;
width(136)->333;
width(137)->1000;
width(138)->556;
width(139)->333;
width(140)->1000;
width(142)->667;
width(145)->333;
width(146)->333;
width(147)->500;
width(148)->500;
width(149)->350;
width(150)->500;
width(151)->1000;
width(152)->333;
width(153)->1000;
width(154)->389;
width(155)->333;
width(156)->722;
width(158)->444;
width(159)->722;
width(161)->333;
width(162)->500;
width(163)->500;
width(164)->500;
width(165)->500;
width(166)->220;
width(167)->500;
width(168)->333;
width(169)->747;
width(170)->300;
width(171)->500;
width(172)->570;
width(173)->570;
width(174)->747;
width(175)->333;
width(176)->400;
width(177)->570;
width(178)->300;
width(179)->300;
width(180)->333;
width(181)->556;
width(182)->540;
width(183)->250;
width(184)->333;
width(185)->300;
width(186)->330;
width(187)->500;
width(188)->750;
width(189)->750;
width(190)->750;
width(191)->500;
width(192)->722;
width(193)->722;
width(194)->722;
width(195)->722;
width(196)->722;
width(197)->722;
width(198)->1000;
width(199)->722;
width(200)->667;
width(201)->667;
width(202)->667;
width(203)->667;
width(204)->389;
width(205)->389;
width(206)->389;
width(207)->389;
width(208)->722;
width(209)->722;
width(210)->778;
width(211)->778;
width(212)->778;
width(213)->778;
width(214)->778;
width(215)->570;
width(216)->778;
width(217)->722;
width(218)->722;
width(219)->722;
width(220)->722;
width(221)->722;
width(222)->611;
width(223)->556;
width(224)->500;
width(225)->500;
width(226)->500;
width(227)->500;
width(228)->500;
width(229)->500;
width(230)->722;
width(231)->444;
width(232)->444;
width(233)->444;
width(234)->444;
width(235)->444;
width(236)->278;
width(237)->278;
width(238)->278;
width(239)->278;
width(240)->500;
width(241)->556;
width(242)->500;
width(243)->500;
width(244)->500;
width(245)->500;
width(246)->500;
width(247)->570;
width(248)->500;
width(249)->556;
width(250)->556;
width(251)->556;
width(252)->556;
width(253)->500;
width(254)->556;
width(255)->500;
width(_)->unknown.
kern(122,101)->0;
kern(122,111)->0;
kern(121,97)->0;
kern(121,44)->-55;
kern(121,101)->-10;
kern(121,111)->-25;
kern(121,46)->-70;
kern(120,101)->0;
kern(119,97)->0;
kern(119,44)->-55;
kern(119,101)->0;
kern(119,104)->0;
kern(119,111)->-10;
kern(119,46)->-70;
kern(118,97)->-10;
kern(118,44)->-55;
kern(118,101)->-10;
kern(118,111)->-10;
kern(118,46)->-70;
kern(32,65)->-55;
kern(32,84)->-30;
kern(32,86)->-45;
kern(32,87)->-30;
kern(32,89)->-55;
kern(32,147)->0;
kern(32,145)->0;
kern(115,119)->0;
kern(114,97)->0;
kern(114,99)->-18;
kern(114,44)->-92;
kern(114,100)->0;
kern(114,101)->-18;
kern(114,103)->-10;
kern(114,45)->-37;
kern(114,105)->0;
kern(114,107)->0;
kern(114,108)->0;
kern(114,109)->0;
kern(114,110)->-15;
kern(114,111)->-18;
kern(114,112)->-10;
kern(114,46)->-100;
kern(114,113)->-18;
kern(114,114)->0;
kern(114,115)->0;
kern(114,116)->0;
kern(114,117)->0;
kern(114,118)->-10;
kern(114,121)->0;
kern(146,100)->-20;
kern(146,108)->0;
kern(146,148)->0;
kern(146,146)->-63;
kern(146,114)->-20;
kern(146,115)->-37;
kern(146,32)->-74;
kern(146,116)->0;
kern(146,118)->-20;
kern(145,65)->-10;
kern(145,145)->-63;
kern(148,32)->0;
kern(147,65)->-10;
kern(147,145)->0;
kern(46,148)->-55;
kern(46,146)->-55;
kern(112,121)->0;
kern(111,103)->0;
kern(111,118)->-10;
kern(111,119)->-10;
kern(111,120)->0;
kern(111,121)->0;
kern(110,117)->0;
kern(110,118)->-40;
kern(110,121)->0;
kern(109,117)->0;
kern(109,121)->0;
kern(108,119)->0;
kern(108,121)->0;
kern(107,101)->-10;
kern(107,111)->-15;
kern(107,121)->-15;
kern(105,118)->-10;
kern(104,121)->-15;
kern(103,97)->0;
kern(103,44)->0;
kern(103,101)->0;
kern(103,103)->0;
kern(103,105)->0;
kern(103,111)->0;
kern(103,46)->-15;
kern(103,114)->0;
kern(103,121)->0;
kern(102,97)->0;
kern(102,44)->-15;
kern(102,101)->0;
kern(102,102)->0;
kern(102,105)->-25;
kern(102,108)->0;
kern(102,111)->-25;
kern(102,46)->-15;
kern(102,148)->50;
kern(102,146)->55;
kern(101,98)->0;
kern(101,44)->0;
kern(101,103)->0;
kern(101,112)->0;
kern(101,46)->0;
kern(101,118)->-15;
kern(101,119)->0;
kern(101,120)->0;
kern(101,121)->0;
kern(100,44)->0;
kern(100,100)->0;
kern(100,46)->0;
kern(100,118)->0;
kern(100,119)->-15;
kern(100,121)->0;
kern(44,148)->-45;
kern(44,146)->-55;
kern(44,32)->0;
kern(58,32)->0;
kern(99,44)->0;
kern(99,104)->0;
kern(99,107)->0;
kern(99,108)->0;
kern(99,46)->0;
kern(99,121)->0;
kern(98,98)->-10;
kern(98,44)->0;
kern(98,108)->0;
kern(98,46)->-40;
kern(98,117)->-20;
kern(98,118)->-15;
kern(98,121)->0;
kern(97,98)->0;
kern(97,103)->0;
kern(97,112)->0;
kern(97,116)->0;
kern(97,118)->-25;
kern(97,119)->0;
kern(97,121)->0;
kern(89,65)->-110;
kern(89,79)->-35;
kern(89,97)->-85;
kern(89,58)->-92;
kern(89,44)->-92;
kern(89,101)->-111;
kern(89,45)->-92;
kern(89,105)->-37;
kern(89,111)->-111;
kern(89,46)->-92;
kern(89,59)->-92;
kern(89,117)->-92;
kern(87,65)->-120;
kern(87,79)->-10;
kern(87,97)->-65;
kern(87,58)->-55;
kern(87,44)->-92;
kern(87,101)->-65;
kern(87,104)->0;
kern(87,45)->-37;
kern(87,105)->-18;
kern(87,111)->-75;
kern(87,46)->-92;
kern(87,59)->-55;
kern(87,117)->-50;
kern(87,121)->-60;
kern(86,65)->-135;
kern(86,71)->-30;
kern(86,79)->-45;
kern(86,97)->-92;
kern(86,58)->-92;
kern(86,44)->-129;
kern(86,101)->-100;
kern(86,45)->-74;
kern(86,105)->-37;
kern(86,111)->-100;
kern(86,46)->-145;
kern(86,59)->-92;
kern(86,117)->-92;
kern(85,65)->-60;
kern(85,44)->-50;
kern(85,46)->-50;
kern(84,65)->-90;
kern(84,79)->-18;
kern(84,97)->-92;
kern(84,58)->-74;
kern(84,44)->-74;
kern(84,101)->-92;
kern(84,104)->0;
kern(84,45)->-92;
kern(84,105)->-18;
kern(84,111)->-92;
kern(84,46)->-90;
kern(84,114)->-74;
kern(84,59)->-74;
kern(84,117)->-92;
kern(84,119)->-74;
kern(84,121)->-74;
kern(83,44)->0;
kern(83,46)->0;
kern(82,79)->-30;
kern(82,84)->-40;
kern(82,85)->-30;
kern(82,86)->-55;
kern(82,87)->-35;
kern(82,89)->-35;
kern(81,85)->-10;
kern(81,44)->0;
kern(81,46)->-20;
kern(80,65)->-74;
kern(80,97)->-10;
kern(80,44)->-92;
kern(80,101)->-20;
kern(80,111)->-20;
kern(80,46)->-110;
kern(79,65)->-40;
kern(79,84)->-40;
kern(79,86)->-50;
kern(79,87)->-50;
kern(79,88)->-40;
kern(79,89)->-50;
kern(79,44)->0;
kern(79,46)->0;
kern(78,65)->-20;
kern(78,44)->0;
kern(78,46)->0;
kern(76,84)->-92;
kern(76,86)->-92;
kern(76,87)->-92;
kern(76,89)->-92;
kern(76,148)->-20;
kern(76,146)->-110;
kern(76,121)->-55;
kern(75,79)->-30;
kern(75,101)->-25;
kern(75,111)->-25;
kern(75,117)->-15;
kern(75,121)->-45;
kern(74,65)->-30;
kern(74,97)->-15;
kern(74,44)->0;
kern(74,101)->-15;
kern(74,111)->-15;
kern(74,46)->-20;
kern(74,117)->-15;
kern(71,44)->0;
kern(71,46)->0;
kern(70,65)->-90;
kern(70,97)->-25;
kern(70,44)->-92;
kern(70,101)->-25;
kern(70,105)->0;
kern(70,111)->-25;
kern(70,46)->-110;
kern(70,114)->0;
kern(68,65)->-35;
kern(68,86)->-40;
kern(68,87)->-40;
kern(68,89)->-40;
kern(68,44)->0;
kern(68,46)->-20;
kern(66,65)->-30;
kern(66,85)->-10;
kern(66,44)->0;
kern(66,46)->0;
kern(65,67)->-55;
kern(65,71)->-55;
kern(65,79)->-45;
kern(65,81)->-45;
kern(65,84)->-95;
kern(65,85)->-50;
kern(65,86)->-145;
kern(65,87)->-130;
kern(65,89)->-100;
kern(65,112)->-25;
kern(65,148)->0;
kern(65,146)->-74;
kern(65,117)->-50;
kern(65,118)->-100;
kern(65,119)->-90;
kern(65,121)->-74;
kern(_,_) -> 0.

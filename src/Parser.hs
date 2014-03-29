{-# OPTIONS_GHC -w #-}
module Parser (parser) where
import Lexer

-- parser produced by Happy Version 1.18.10

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26
	= HappyTerminal (Lexeme)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 t16
	| HappyAbsSyn17 t17
	| HappyAbsSyn18 t18
	| HappyAbsSyn19 t19
	| HappyAbsSyn20 t20
	| HappyAbsSyn21 t21
	| HappyAbsSyn22 t22
	| HappyAbsSyn23 t23
	| HappyAbsSyn24 t24
	| HappyAbsSyn25 t25
	| HappyAbsSyn26 t26

action_0 (34) = happyShift action_4
action_0 (4) = happyGoto action_5
action_0 (5) = happyGoto action_2
action_0 (6) = happyGoto action_3
action_0 _ = happyFail

action_1 (34) = happyShift action_4
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 _ = happyFail

action_2 _ = happyReduce_1

action_3 _ = happyReduce_3

action_4 (30) = happyShift action_7
action_4 _ = happyFail

action_5 (34) = happyShift action_4
action_5 (100) = happyAccept
action_5 (5) = happyGoto action_6
action_5 (6) = happyGoto action_3
action_5 _ = happyFail

action_6 _ = happyReduce_2

action_7 (64) = happyShift action_8
action_7 _ = happyFail

action_8 (30) = happyShift action_11
action_8 (7) = happyGoto action_9
action_8 (8) = happyGoto action_10
action_8 _ = happyFail

action_9 (65) = happyShift action_14
action_9 (68) = happyShift action_15
action_9 _ = happyFail

action_10 _ = happyReduce_7

action_11 (57) = happyShift action_13
action_11 (24) = happyGoto action_12
action_11 _ = happyReduce_75

action_12 (58) = happyShift action_32
action_12 _ = happyFail

action_13 (27) = happyShift action_20
action_13 (28) = happyShift action_21
action_13 (29) = happyShift action_22
action_13 (30) = happyShift action_23
action_13 (31) = happyShift action_24
action_13 (55) = happyShift action_25
action_13 (64) = happyShift action_26
action_13 (66) = happyShift action_27
action_13 (77) = happyShift action_28
action_13 (78) = happyShift action_29
action_13 (98) = happyShift action_30
action_13 (99) = happyShift action_31
action_13 (14) = happyGoto action_19
action_13 _ = happyFail

action_14 (58) = happyShift action_17
action_14 (62) = happyShift action_18
action_14 _ = happyFail

action_15 (30) = happyShift action_11
action_15 (8) = happyGoto action_16
action_15 _ = happyFail

action_16 _ = happyReduce_6

action_17 (30) = happyShift action_34
action_17 (9) = happyGoto action_80
action_17 _ = happyFail

action_18 (35) = happyShift action_74
action_18 (37) = happyShift action_75
action_18 (38) = happyShift action_76
action_18 (50) = happyShift action_77
action_18 (51) = happyShift action_78
action_18 (52) = happyShift action_79
action_18 (11) = happyGoto action_66
action_18 (12) = happyGoto action_67
action_18 (13) = happyGoto action_68
action_18 (16) = happyGoto action_69
action_18 (17) = happyGoto action_70
action_18 (18) = happyGoto action_71
action_18 (25) = happyGoto action_72
action_18 (26) = happyGoto action_73
action_18 _ = happyFail

action_19 (43) = happyShift action_42
action_19 (53) = happyShift action_43
action_19 (54) = happyShift action_44
action_19 (56) = happyShift action_45
action_19 (60) = happyShift action_46
action_19 (64) = happyShift action_47
action_19 (67) = happyShift action_48
action_19 (69) = happyShift action_49
action_19 (70) = happyShift action_50
action_19 (71) = happyShift action_51
action_19 (72) = happyShift action_52
action_19 (73) = happyShift action_53
action_19 (74) = happyShift action_54
action_19 (75) = happyShift action_55
action_19 (76) = happyShift action_56
action_19 (77) = happyShift action_57
action_19 (78) = happyShift action_58
action_19 (79) = happyShift action_59
action_19 (80) = happyShift action_60
action_19 (81) = happyShift action_61
action_19 (82) = happyShift action_62
action_19 (83) = happyShift action_63
action_19 (96) = happyShift action_64
action_19 (97) = happyShift action_65
action_19 _ = happyReduce_74

action_20 _ = happyReduce_25

action_21 _ = happyReduce_24

action_22 _ = happyReduce_26

action_23 _ = happyReduce_28

action_24 _ = happyReduce_27

action_25 (27) = happyShift action_20
action_25 (28) = happyShift action_21
action_25 (29) = happyShift action_22
action_25 (30) = happyShift action_23
action_25 (31) = happyShift action_24
action_25 (55) = happyShift action_25
action_25 (64) = happyShift action_26
action_25 (66) = happyShift action_27
action_25 (77) = happyShift action_28
action_25 (78) = happyShift action_29
action_25 (98) = happyShift action_30
action_25 (99) = happyShift action_31
action_25 (14) = happyGoto action_41
action_25 _ = happyFail

action_26 (27) = happyShift action_20
action_26 (28) = happyShift action_21
action_26 (29) = happyShift action_22
action_26 (30) = happyShift action_23
action_26 (31) = happyShift action_24
action_26 (55) = happyShift action_25
action_26 (64) = happyShift action_26
action_26 (66) = happyShift action_27
action_26 (77) = happyShift action_28
action_26 (78) = happyShift action_29
action_26 (98) = happyShift action_30
action_26 (99) = happyShift action_31
action_26 (14) = happyGoto action_40
action_26 _ = happyFail

action_27 (27) = happyShift action_20
action_27 (28) = happyShift action_21
action_27 (29) = happyShift action_22
action_27 (30) = happyShift action_23
action_27 (31) = happyShift action_24
action_27 (55) = happyShift action_25
action_27 (64) = happyShift action_26
action_27 (66) = happyShift action_27
action_27 (77) = happyShift action_28
action_27 (78) = happyShift action_29
action_27 (98) = happyShift action_30
action_27 (99) = happyShift action_31
action_27 (14) = happyGoto action_39
action_27 _ = happyFail

action_28 (27) = happyShift action_20
action_28 (28) = happyShift action_21
action_28 (29) = happyShift action_22
action_28 (30) = happyShift action_23
action_28 (31) = happyShift action_24
action_28 (55) = happyShift action_25
action_28 (64) = happyShift action_26
action_28 (66) = happyShift action_27
action_28 (77) = happyShift action_28
action_28 (78) = happyShift action_29
action_28 (98) = happyShift action_30
action_28 (99) = happyShift action_31
action_28 (14) = happyGoto action_38
action_28 _ = happyFail

action_29 (27) = happyShift action_20
action_29 (28) = happyShift action_21
action_29 (29) = happyShift action_22
action_29 (30) = happyShift action_23
action_29 (31) = happyShift action_24
action_29 (55) = happyShift action_25
action_29 (64) = happyShift action_26
action_29 (66) = happyShift action_27
action_29 (77) = happyShift action_28
action_29 (78) = happyShift action_29
action_29 (98) = happyShift action_30
action_29 (99) = happyShift action_31
action_29 (14) = happyGoto action_37
action_29 _ = happyFail

action_30 (27) = happyShift action_20
action_30 (28) = happyShift action_21
action_30 (29) = happyShift action_22
action_30 (30) = happyShift action_23
action_30 (31) = happyShift action_24
action_30 (55) = happyShift action_25
action_30 (64) = happyShift action_26
action_30 (66) = happyShift action_27
action_30 (77) = happyShift action_28
action_30 (78) = happyShift action_29
action_30 (98) = happyShift action_30
action_30 (99) = happyShift action_31
action_30 (14) = happyGoto action_36
action_30 _ = happyFail

action_31 (27) = happyShift action_20
action_31 (28) = happyShift action_21
action_31 (29) = happyShift action_22
action_31 (30) = happyShift action_23
action_31 (31) = happyShift action_24
action_31 (55) = happyShift action_25
action_31 (64) = happyShift action_26
action_31 (66) = happyShift action_27
action_31 (77) = happyShift action_28
action_31 (78) = happyShift action_29
action_31 (98) = happyShift action_30
action_31 (99) = happyShift action_31
action_31 (14) = happyGoto action_35
action_31 _ = happyFail

action_32 (30) = happyShift action_34
action_32 (9) = happyGoto action_33
action_32 _ = happyFail

action_33 _ = happyReduce_8

action_34 (60) = happyShift action_117
action_34 (10) = happyGoto action_116
action_34 _ = happyReduce_9

action_35 (43) = happyShift action_42
action_35 (60) = happyShift action_46
action_35 (64) = happyShift action_47
action_35 (69) = happyShift action_49
action_35 (81) = happyShift action_61
action_35 _ = happyReduce_55

action_36 (43) = happyShift action_42
action_36 (60) = happyShift action_46
action_36 (64) = happyShift action_47
action_36 (69) = happyShift action_49
action_36 (81) = happyShift action_61
action_36 _ = happyReduce_56

action_37 (43) = happyShift action_42
action_37 (60) = happyShift action_46
action_37 (64) = happyShift action_47
action_37 (69) = happyShift action_49
action_37 (81) = happyShift action_61
action_37 _ = happyReduce_57

action_38 (43) = happyShift action_42
action_38 (60) = happyShift action_46
action_38 (64) = happyShift action_47
action_38 (69) = happyShift action_49
action_38 (81) = happyShift action_61
action_38 _ = happyReduce_58

action_39 (43) = happyShift action_42
action_39 (60) = happyShift action_46
action_39 (64) = happyShift action_47
action_39 (69) = happyShift action_49
action_39 (81) = happyShift action_61
action_39 _ = happyReduce_54

action_40 (43) = happyShift action_42
action_40 (53) = happyShift action_43
action_40 (54) = happyShift action_44
action_40 (56) = happyShift action_45
action_40 (60) = happyShift action_46
action_40 (64) = happyShift action_47
action_40 (65) = happyShift action_115
action_40 (67) = happyShift action_48
action_40 (69) = happyShift action_49
action_40 (70) = happyShift action_50
action_40 (71) = happyShift action_51
action_40 (72) = happyShift action_52
action_40 (73) = happyShift action_53
action_40 (74) = happyShift action_54
action_40 (75) = happyShift action_55
action_40 (76) = happyShift action_56
action_40 (77) = happyShift action_57
action_40 (78) = happyShift action_58
action_40 (79) = happyShift action_59
action_40 (80) = happyShift action_60
action_40 (81) = happyShift action_61
action_40 (82) = happyShift action_62
action_40 (83) = happyShift action_63
action_40 (96) = happyShift action_64
action_40 (97) = happyShift action_65
action_40 _ = happyFail

action_41 (43) = happyShift action_42
action_41 (60) = happyShift action_46
action_41 (64) = happyShift action_47
action_41 (69) = happyShift action_49
action_41 (81) = happyShift action_61
action_41 _ = happyReduce_59

action_42 (30) = happyShift action_114
action_42 _ = happyFail

action_43 (27) = happyShift action_20
action_43 (28) = happyShift action_21
action_43 (29) = happyShift action_22
action_43 (30) = happyShift action_23
action_43 (31) = happyShift action_24
action_43 (55) = happyShift action_25
action_43 (64) = happyShift action_26
action_43 (66) = happyShift action_27
action_43 (77) = happyShift action_28
action_43 (78) = happyShift action_29
action_43 (98) = happyShift action_30
action_43 (99) = happyShift action_31
action_43 (14) = happyGoto action_113
action_43 _ = happyFail

action_44 (27) = happyShift action_20
action_44 (28) = happyShift action_21
action_44 (29) = happyShift action_22
action_44 (30) = happyShift action_23
action_44 (31) = happyShift action_24
action_44 (55) = happyShift action_25
action_44 (64) = happyShift action_26
action_44 (66) = happyShift action_27
action_44 (77) = happyShift action_28
action_44 (78) = happyShift action_29
action_44 (98) = happyShift action_30
action_44 (99) = happyShift action_31
action_44 (14) = happyGoto action_112
action_44 _ = happyFail

action_45 (27) = happyShift action_20
action_45 (28) = happyShift action_21
action_45 (29) = happyShift action_22
action_45 (30) = happyShift action_23
action_45 (31) = happyShift action_24
action_45 (55) = happyShift action_25
action_45 (64) = happyShift action_26
action_45 (66) = happyShift action_27
action_45 (77) = happyShift action_28
action_45 (78) = happyShift action_29
action_45 (98) = happyShift action_30
action_45 (99) = happyShift action_31
action_45 (14) = happyGoto action_111
action_45 _ = happyFail

action_46 (27) = happyShift action_20
action_46 (28) = happyShift action_21
action_46 (29) = happyShift action_22
action_46 (30) = happyShift action_23
action_46 (31) = happyShift action_24
action_46 (55) = happyShift action_25
action_46 (64) = happyShift action_26
action_46 (66) = happyShift action_27
action_46 (77) = happyShift action_28
action_46 (78) = happyShift action_29
action_46 (98) = happyShift action_30
action_46 (99) = happyShift action_31
action_46 (14) = happyGoto action_110
action_46 _ = happyFail

action_47 (27) = happyShift action_20
action_47 (28) = happyShift action_21
action_47 (29) = happyShift action_22
action_47 (30) = happyShift action_23
action_47 (31) = happyShift action_24
action_47 (55) = happyShift action_25
action_47 (64) = happyShift action_26
action_47 (66) = happyShift action_27
action_47 (77) = happyShift action_28
action_47 (78) = happyShift action_29
action_47 (98) = happyShift action_30
action_47 (99) = happyShift action_31
action_47 (14) = happyGoto action_84
action_47 (15) = happyGoto action_109
action_47 _ = happyFail

action_48 (27) = happyShift action_20
action_48 (28) = happyShift action_21
action_48 (29) = happyShift action_22
action_48 (30) = happyShift action_23
action_48 (31) = happyShift action_24
action_48 (55) = happyShift action_25
action_48 (64) = happyShift action_26
action_48 (66) = happyShift action_27
action_48 (77) = happyShift action_28
action_48 (78) = happyShift action_29
action_48 (98) = happyShift action_30
action_48 (99) = happyShift action_31
action_48 (14) = happyGoto action_108
action_48 _ = happyFail

action_49 (27) = happyShift action_20
action_49 (28) = happyShift action_21
action_49 (29) = happyShift action_22
action_49 (30) = happyShift action_23
action_49 (31) = happyShift action_24
action_49 (55) = happyShift action_25
action_49 (64) = happyShift action_26
action_49 (66) = happyShift action_27
action_49 (77) = happyShift action_28
action_49 (78) = happyShift action_29
action_49 (98) = happyShift action_30
action_49 (99) = happyShift action_31
action_49 (14) = happyGoto action_107
action_49 _ = happyFail

action_50 (27) = happyShift action_20
action_50 (28) = happyShift action_21
action_50 (29) = happyShift action_22
action_50 (30) = happyShift action_23
action_50 (31) = happyShift action_24
action_50 (55) = happyShift action_25
action_50 (64) = happyShift action_26
action_50 (66) = happyShift action_27
action_50 (77) = happyShift action_28
action_50 (78) = happyShift action_29
action_50 (98) = happyShift action_30
action_50 (99) = happyShift action_31
action_50 (14) = happyGoto action_106
action_50 _ = happyFail

action_51 (27) = happyShift action_20
action_51 (28) = happyShift action_21
action_51 (29) = happyShift action_22
action_51 (30) = happyShift action_23
action_51 (31) = happyShift action_24
action_51 (55) = happyShift action_25
action_51 (64) = happyShift action_26
action_51 (66) = happyShift action_27
action_51 (77) = happyShift action_28
action_51 (78) = happyShift action_29
action_51 (98) = happyShift action_30
action_51 (99) = happyShift action_31
action_51 (14) = happyGoto action_105
action_51 _ = happyFail

action_52 (27) = happyShift action_20
action_52 (28) = happyShift action_21
action_52 (29) = happyShift action_22
action_52 (30) = happyShift action_23
action_52 (31) = happyShift action_24
action_52 (55) = happyShift action_25
action_52 (64) = happyShift action_26
action_52 (66) = happyShift action_27
action_52 (77) = happyShift action_28
action_52 (78) = happyShift action_29
action_52 (98) = happyShift action_30
action_52 (99) = happyShift action_31
action_52 (14) = happyGoto action_104
action_52 _ = happyFail

action_53 (27) = happyShift action_20
action_53 (28) = happyShift action_21
action_53 (29) = happyShift action_22
action_53 (30) = happyShift action_23
action_53 (31) = happyShift action_24
action_53 (55) = happyShift action_25
action_53 (64) = happyShift action_26
action_53 (66) = happyShift action_27
action_53 (77) = happyShift action_28
action_53 (78) = happyShift action_29
action_53 (98) = happyShift action_30
action_53 (99) = happyShift action_31
action_53 (14) = happyGoto action_103
action_53 _ = happyFail

action_54 (27) = happyShift action_20
action_54 (28) = happyShift action_21
action_54 (29) = happyShift action_22
action_54 (30) = happyShift action_23
action_54 (31) = happyShift action_24
action_54 (55) = happyShift action_25
action_54 (64) = happyShift action_26
action_54 (66) = happyShift action_27
action_54 (77) = happyShift action_28
action_54 (78) = happyShift action_29
action_54 (98) = happyShift action_30
action_54 (99) = happyShift action_31
action_54 (14) = happyGoto action_102
action_54 _ = happyFail

action_55 (27) = happyShift action_20
action_55 (28) = happyShift action_21
action_55 (29) = happyShift action_22
action_55 (30) = happyShift action_23
action_55 (31) = happyShift action_24
action_55 (55) = happyShift action_25
action_55 (64) = happyShift action_26
action_55 (66) = happyShift action_27
action_55 (77) = happyShift action_28
action_55 (78) = happyShift action_29
action_55 (98) = happyShift action_30
action_55 (99) = happyShift action_31
action_55 (14) = happyGoto action_101
action_55 _ = happyFail

action_56 (27) = happyShift action_20
action_56 (28) = happyShift action_21
action_56 (29) = happyShift action_22
action_56 (30) = happyShift action_23
action_56 (31) = happyShift action_24
action_56 (55) = happyShift action_25
action_56 (64) = happyShift action_26
action_56 (66) = happyShift action_27
action_56 (77) = happyShift action_28
action_56 (78) = happyShift action_29
action_56 (98) = happyShift action_30
action_56 (99) = happyShift action_31
action_56 (14) = happyGoto action_100
action_56 _ = happyFail

action_57 (27) = happyShift action_20
action_57 (28) = happyShift action_21
action_57 (29) = happyShift action_22
action_57 (30) = happyShift action_23
action_57 (31) = happyShift action_24
action_57 (55) = happyShift action_25
action_57 (64) = happyShift action_26
action_57 (66) = happyShift action_27
action_57 (77) = happyShift action_28
action_57 (78) = happyShift action_29
action_57 (98) = happyShift action_30
action_57 (99) = happyShift action_31
action_57 (14) = happyGoto action_99
action_57 _ = happyFail

action_58 (27) = happyShift action_20
action_58 (28) = happyShift action_21
action_58 (29) = happyShift action_22
action_58 (30) = happyShift action_23
action_58 (31) = happyShift action_24
action_58 (55) = happyShift action_25
action_58 (64) = happyShift action_26
action_58 (66) = happyShift action_27
action_58 (77) = happyShift action_28
action_58 (78) = happyShift action_29
action_58 (98) = happyShift action_30
action_58 (99) = happyShift action_31
action_58 (14) = happyGoto action_98
action_58 _ = happyFail

action_59 (27) = happyShift action_20
action_59 (28) = happyShift action_21
action_59 (29) = happyShift action_22
action_59 (30) = happyShift action_23
action_59 (31) = happyShift action_24
action_59 (55) = happyShift action_25
action_59 (64) = happyShift action_26
action_59 (66) = happyShift action_27
action_59 (77) = happyShift action_28
action_59 (78) = happyShift action_29
action_59 (98) = happyShift action_30
action_59 (99) = happyShift action_31
action_59 (14) = happyGoto action_97
action_59 _ = happyFail

action_60 (27) = happyShift action_20
action_60 (28) = happyShift action_21
action_60 (29) = happyShift action_22
action_60 (30) = happyShift action_23
action_60 (31) = happyShift action_24
action_60 (55) = happyShift action_25
action_60 (64) = happyShift action_26
action_60 (66) = happyShift action_27
action_60 (77) = happyShift action_28
action_60 (78) = happyShift action_29
action_60 (98) = happyShift action_30
action_60 (99) = happyShift action_31
action_60 (14) = happyGoto action_96
action_60 _ = happyFail

action_61 (27) = happyShift action_20
action_61 (28) = happyShift action_21
action_61 (29) = happyShift action_22
action_61 (30) = happyShift action_23
action_61 (31) = happyShift action_24
action_61 (55) = happyShift action_25
action_61 (64) = happyShift action_26
action_61 (66) = happyShift action_27
action_61 (77) = happyShift action_28
action_61 (78) = happyShift action_29
action_61 (98) = happyShift action_30
action_61 (99) = happyShift action_31
action_61 (14) = happyGoto action_95
action_61 _ = happyFail

action_62 (27) = happyShift action_20
action_62 (28) = happyShift action_21
action_62 (29) = happyShift action_22
action_62 (30) = happyShift action_23
action_62 (31) = happyShift action_24
action_62 (55) = happyShift action_25
action_62 (64) = happyShift action_26
action_62 (66) = happyShift action_27
action_62 (77) = happyShift action_28
action_62 (78) = happyShift action_29
action_62 (98) = happyShift action_30
action_62 (99) = happyShift action_31
action_62 (14) = happyGoto action_94
action_62 _ = happyFail

action_63 (27) = happyShift action_20
action_63 (28) = happyShift action_21
action_63 (29) = happyShift action_22
action_63 (30) = happyShift action_23
action_63 (31) = happyShift action_24
action_63 (55) = happyShift action_25
action_63 (64) = happyShift action_26
action_63 (66) = happyShift action_27
action_63 (77) = happyShift action_28
action_63 (78) = happyShift action_29
action_63 (98) = happyShift action_30
action_63 (99) = happyShift action_31
action_63 (14) = happyGoto action_93
action_63 _ = happyFail

action_64 (27) = happyShift action_20
action_64 (28) = happyShift action_21
action_64 (29) = happyShift action_22
action_64 (30) = happyShift action_23
action_64 (31) = happyShift action_24
action_64 (55) = happyShift action_25
action_64 (64) = happyShift action_26
action_64 (66) = happyShift action_27
action_64 (77) = happyShift action_28
action_64 (78) = happyShift action_29
action_64 (98) = happyShift action_30
action_64 (99) = happyShift action_31
action_64 (14) = happyGoto action_92
action_64 _ = happyFail

action_65 (27) = happyShift action_20
action_65 (28) = happyShift action_21
action_65 (29) = happyShift action_22
action_65 (30) = happyShift action_23
action_65 (31) = happyShift action_24
action_65 (55) = happyShift action_25
action_65 (64) = happyShift action_26
action_65 (66) = happyShift action_27
action_65 (77) = happyShift action_28
action_65 (78) = happyShift action_29
action_65 (98) = happyShift action_30
action_65 (99) = happyShift action_31
action_65 (14) = happyGoto action_91
action_65 _ = happyFail

action_66 (35) = happyShift action_74
action_66 (37) = happyShift action_75
action_66 (38) = happyShift action_76
action_66 (50) = happyShift action_77
action_66 (51) = happyShift action_78
action_66 (52) = happyShift action_79
action_66 (63) = happyShift action_90
action_66 (12) = happyGoto action_89
action_66 (13) = happyGoto action_68
action_66 (16) = happyGoto action_69
action_66 (17) = happyGoto action_70
action_66 (18) = happyGoto action_71
action_66 (25) = happyGoto action_72
action_66 (26) = happyGoto action_73
action_66 _ = happyFail

action_67 (59) = happyShift action_88
action_67 _ = happyFail

action_68 _ = happyReduce_17

action_69 _ = happyReduce_18

action_70 _ = happyReduce_19

action_71 _ = happyReduce_20

action_72 _ = happyReduce_21

action_73 _ = happyReduce_22

action_74 (30) = happyShift action_87
action_74 (23) = happyGoto action_86
action_74 _ = happyFail

action_75 (27) = happyShift action_20
action_75 (28) = happyShift action_21
action_75 (29) = happyShift action_22
action_75 (30) = happyShift action_23
action_75 (31) = happyShift action_24
action_75 (55) = happyShift action_25
action_75 (64) = happyShift action_26
action_75 (66) = happyShift action_27
action_75 (77) = happyShift action_28
action_75 (78) = happyShift action_29
action_75 (98) = happyShift action_30
action_75 (99) = happyShift action_31
action_75 (14) = happyGoto action_84
action_75 (15) = happyGoto action_85
action_75 _ = happyFail

action_76 (27) = happyShift action_20
action_76 (28) = happyShift action_21
action_76 (29) = happyShift action_22
action_76 (30) = happyShift action_23
action_76 (31) = happyShift action_24
action_76 (55) = happyShift action_25
action_76 (64) = happyShift action_26
action_76 (66) = happyShift action_27
action_76 (77) = happyShift action_28
action_76 (78) = happyShift action_29
action_76 (98) = happyShift action_30
action_76 (99) = happyShift action_31
action_76 (14) = happyGoto action_83
action_76 _ = happyFail

action_77 _ = happyReduce_65

action_78 _ = happyReduce_64

action_79 (27) = happyShift action_20
action_79 (28) = happyShift action_21
action_79 (29) = happyShift action_22
action_79 (30) = happyShift action_23
action_79 (31) = happyShift action_24
action_79 (55) = happyShift action_25
action_79 (64) = happyShift action_26
action_79 (66) = happyShift action_27
action_79 (77) = happyShift action_28
action_79 (78) = happyShift action_29
action_79 (98) = happyShift action_30
action_79 (99) = happyShift action_31
action_79 (14) = happyGoto action_82
action_79 _ = happyFail

action_80 (62) = happyShift action_81
action_80 _ = happyFail

action_81 (35) = happyShift action_74
action_81 (37) = happyShift action_75
action_81 (38) = happyShift action_76
action_81 (50) = happyShift action_77
action_81 (51) = happyShift action_78
action_81 (52) = happyShift action_79
action_81 (11) = happyGoto action_129
action_81 (12) = happyGoto action_67
action_81 (13) = happyGoto action_68
action_81 (16) = happyGoto action_69
action_81 (17) = happyGoto action_70
action_81 (18) = happyGoto action_71
action_81 (25) = happyGoto action_72
action_81 (26) = happyGoto action_73
action_81 _ = happyFail

action_82 (43) = happyShift action_42
action_82 (53) = happyShift action_43
action_82 (54) = happyShift action_44
action_82 (56) = happyShift action_45
action_82 (60) = happyShift action_46
action_82 (64) = happyShift action_47
action_82 (67) = happyShift action_48
action_82 (69) = happyShift action_49
action_82 (70) = happyShift action_50
action_82 (71) = happyShift action_51
action_82 (72) = happyShift action_52
action_82 (73) = happyShift action_53
action_82 (74) = happyShift action_54
action_82 (75) = happyShift action_55
action_82 (76) = happyShift action_56
action_82 (77) = happyShift action_57
action_82 (78) = happyShift action_58
action_82 (79) = happyShift action_59
action_82 (80) = happyShift action_60
action_82 (81) = happyShift action_61
action_82 (82) = happyShift action_62
action_82 (83) = happyShift action_63
action_82 (96) = happyShift action_64
action_82 (97) = happyShift action_65
action_82 _ = happyReduce_63

action_83 (43) = happyShift action_42
action_83 (53) = happyShift action_43
action_83 (54) = happyShift action_44
action_83 (56) = happyShift action_45
action_83 (60) = happyShift action_46
action_83 (64) = happyShift action_47
action_83 (67) = happyShift action_48
action_83 (69) = happyShift action_49
action_83 (70) = happyShift action_50
action_83 (71) = happyShift action_51
action_83 (72) = happyShift action_52
action_83 (73) = happyShift action_53
action_83 (74) = happyShift action_54
action_83 (75) = happyShift action_55
action_83 (76) = happyShift action_56
action_83 (77) = happyShift action_57
action_83 (78) = happyShift action_58
action_83 (79) = happyShift action_59
action_83 (80) = happyShift action_60
action_83 (81) = happyShift action_61
action_83 (82) = happyShift action_62
action_83 (83) = happyShift action_63
action_83 (96) = happyShift action_64
action_83 (97) = happyShift action_65
action_83 _ = happyReduce_77

action_84 (43) = happyShift action_42
action_84 (53) = happyShift action_43
action_84 (54) = happyShift action_44
action_84 (56) = happyShift action_45
action_84 (60) = happyShift action_46
action_84 (64) = happyShift action_47
action_84 (67) = happyShift action_48
action_84 (69) = happyShift action_49
action_84 (70) = happyShift action_50
action_84 (71) = happyShift action_51
action_84 (72) = happyShift action_52
action_84 (73) = happyShift action_53
action_84 (74) = happyShift action_54
action_84 (75) = happyShift action_55
action_84 (76) = happyShift action_56
action_84 (77) = happyShift action_57
action_84 (78) = happyShift action_58
action_84 (79) = happyShift action_59
action_84 (80) = happyShift action_60
action_84 (81) = happyShift action_61
action_84 (82) = happyShift action_62
action_84 (83) = happyShift action_63
action_84 (96) = happyShift action_64
action_84 (97) = happyShift action_65
action_84 _ = happyReduce_62

action_85 (68) = happyShift action_123
action_85 _ = happyReduce_76

action_86 (58) = happyShift action_127
action_86 (68) = happyShift action_128
action_86 _ = happyFail

action_87 (57) = happyShift action_13
action_87 (24) = happyGoto action_126
action_87 _ = happyReduce_75

action_88 _ = happyReduce_15

action_89 (59) = happyShift action_125
action_89 _ = happyFail

action_90 _ = happyReduce_5

action_91 (43) = happyShift action_42
action_91 (60) = happyShift action_46
action_91 (64) = happyShift action_47
action_91 (67) = happyShift action_48
action_91 (69) = happyShift action_49
action_91 (71) = happyShift action_51
action_91 (72) = happyShift action_52
action_91 (73) = happyShift action_53
action_91 (74) = happyShift action_54
action_91 (75) = happyShift action_55
action_91 (76) = happyShift action_56
action_91 (77) = happyShift action_57
action_91 (78) = happyShift action_58
action_91 (79) = happyShift action_59
action_91 (80) = happyShift action_60
action_91 (81) = happyShift action_61
action_91 (82) = happyShift action_62
action_91 (83) = happyShift action_63
action_91 _ = happyReduce_50

action_92 (43) = happyShift action_42
action_92 (56) = happyShift action_45
action_92 (60) = happyShift action_46
action_92 (64) = happyShift action_47
action_92 (67) = happyShift action_48
action_92 (69) = happyShift action_49
action_92 (71) = happyShift action_51
action_92 (72) = happyShift action_52
action_92 (73) = happyShift action_53
action_92 (74) = happyShift action_54
action_92 (75) = happyShift action_55
action_92 (76) = happyShift action_56
action_92 (77) = happyShift action_57
action_92 (78) = happyShift action_58
action_92 (79) = happyShift action_59
action_92 (80) = happyShift action_60
action_92 (81) = happyShift action_61
action_92 (82) = happyShift action_62
action_92 (83) = happyShift action_63
action_92 (97) = happyShift action_65
action_92 _ = happyReduce_49

action_93 (43) = happyShift action_42
action_93 (60) = happyShift action_46
action_93 (64) = happyShift action_47
action_93 (67) = happyShift action_48
action_93 (69) = happyShift action_49
action_93 (77) = happyShift action_57
action_93 (78) = happyShift action_58
action_93 (79) = happyShift action_59
action_93 (80) = happyShift action_60
action_93 (81) = happyShift action_61
action_93 _ = happyReduce_48

action_94 (43) = happyShift action_42
action_94 (60) = happyShift action_46
action_94 (64) = happyShift action_47
action_94 (67) = happyShift action_48
action_94 (69) = happyShift action_49
action_94 (77) = happyShift action_57
action_94 (78) = happyShift action_58
action_94 (79) = happyShift action_59
action_94 (80) = happyShift action_60
action_94 (81) = happyShift action_61
action_94 _ = happyReduce_47

action_95 (60) = happyShift action_46
action_95 (64) = happyShift action_47
action_95 (69) = happyShift action_49
action_95 _ = happyReduce_46

action_96 (43) = happyShift action_42
action_96 (60) = happyShift action_46
action_96 (64) = happyShift action_47
action_96 (69) = happyShift action_49
action_96 (81) = happyShift action_61
action_96 _ = happyReduce_36

action_97 (43) = happyShift action_42
action_97 (60) = happyShift action_46
action_97 (64) = happyShift action_47
action_97 (69) = happyShift action_49
action_97 (81) = happyShift action_61
action_97 _ = happyReduce_35

action_98 (43) = happyShift action_42
action_98 (60) = happyShift action_46
action_98 (64) = happyShift action_47
action_98 (67) = happyShift action_48
action_98 (69) = happyShift action_49
action_98 (79) = happyShift action_59
action_98 (80) = happyShift action_60
action_98 (81) = happyShift action_61
action_98 _ = happyReduce_34

action_99 (43) = happyShift action_42
action_99 (60) = happyShift action_46
action_99 (64) = happyShift action_47
action_99 (67) = happyShift action_48
action_99 (69) = happyShift action_49
action_99 (79) = happyShift action_59
action_99 (80) = happyShift action_60
action_99 (81) = happyShift action_61
action_99 _ = happyReduce_33

action_100 (43) = happyShift action_42
action_100 (60) = happyShift action_46
action_100 (64) = happyShift action_47
action_100 (67) = happyShift action_48
action_100 (69) = happyShift action_49
action_100 (77) = happyShift action_57
action_100 (78) = happyShift action_58
action_100 (79) = happyShift action_59
action_100 (80) = happyShift action_60
action_100 (81) = happyShift action_61
action_100 (82) = happyShift action_62
action_100 (83) = happyShift action_63
action_100 _ = happyReduce_45

action_101 (43) = happyShift action_42
action_101 (60) = happyShift action_46
action_101 (64) = happyShift action_47
action_101 (67) = happyShift action_48
action_101 (69) = happyShift action_49
action_101 (77) = happyShift action_57
action_101 (78) = happyShift action_58
action_101 (79) = happyShift action_59
action_101 (80) = happyShift action_60
action_101 (81) = happyShift action_61
action_101 (82) = happyShift action_62
action_101 (83) = happyShift action_63
action_101 _ = happyReduce_44

action_102 (43) = happyShift action_42
action_102 (60) = happyShift action_46
action_102 (64) = happyShift action_47
action_102 (67) = happyShift action_48
action_102 (69) = happyShift action_49
action_102 (77) = happyShift action_57
action_102 (78) = happyShift action_58
action_102 (79) = happyShift action_59
action_102 (80) = happyShift action_60
action_102 (81) = happyShift action_61
action_102 (82) = happyShift action_62
action_102 (83) = happyShift action_63
action_102 _ = happyReduce_43

action_103 (43) = happyShift action_42
action_103 (60) = happyShift action_46
action_103 (64) = happyShift action_47
action_103 (67) = happyShift action_48
action_103 (69) = happyShift action_49
action_103 (77) = happyShift action_57
action_103 (78) = happyShift action_58
action_103 (79) = happyShift action_59
action_103 (80) = happyShift action_60
action_103 (81) = happyShift action_61
action_103 (82) = happyShift action_62
action_103 (83) = happyShift action_63
action_103 _ = happyReduce_42

action_104 (43) = happyShift action_42
action_104 (60) = happyShift action_46
action_104 (64) = happyShift action_47
action_104 (67) = happyShift action_48
action_104 (69) = happyShift action_49
action_104 (73) = happyShift action_53
action_104 (74) = happyShift action_54
action_104 (75) = happyShift action_55
action_104 (76) = happyShift action_56
action_104 (77) = happyShift action_57
action_104 (78) = happyShift action_58
action_104 (79) = happyShift action_59
action_104 (80) = happyShift action_60
action_104 (81) = happyShift action_61
action_104 (82) = happyShift action_62
action_104 (83) = happyShift action_63
action_104 _ = happyReduce_41

action_105 (43) = happyShift action_42
action_105 (60) = happyShift action_46
action_105 (64) = happyShift action_47
action_105 (67) = happyShift action_48
action_105 (69) = happyShift action_49
action_105 (73) = happyShift action_53
action_105 (74) = happyShift action_54
action_105 (75) = happyShift action_55
action_105 (76) = happyShift action_56
action_105 (77) = happyShift action_57
action_105 (78) = happyShift action_58
action_105 (79) = happyShift action_59
action_105 (80) = happyShift action_60
action_105 (81) = happyShift action_61
action_105 (82) = happyShift action_62
action_105 (83) = happyShift action_63
action_105 _ = happyReduce_40

action_106 (36) = happyShift action_124
action_106 (43) = happyShift action_42
action_106 (53) = happyShift action_43
action_106 (54) = happyShift action_44
action_106 (56) = happyShift action_45
action_106 (60) = happyShift action_46
action_106 (64) = happyShift action_47
action_106 (67) = happyShift action_48
action_106 (69) = happyShift action_49
action_106 (71) = happyShift action_51
action_106 (72) = happyShift action_52
action_106 (73) = happyShift action_53
action_106 (74) = happyShift action_54
action_106 (75) = happyShift action_55
action_106 (76) = happyShift action_56
action_106 (77) = happyShift action_57
action_106 (78) = happyShift action_58
action_106 (79) = happyShift action_59
action_106 (80) = happyShift action_60
action_106 (81) = happyShift action_61
action_106 (82) = happyShift action_62
action_106 (83) = happyShift action_63
action_106 (96) = happyShift action_64
action_106 (97) = happyShift action_65
action_106 _ = happyReduce_52

action_107 _ = happyReduce_30

action_108 (43) = happyShift action_42
action_108 (60) = happyShift action_46
action_108 (64) = happyShift action_47
action_108 (69) = happyShift action_49
action_108 (81) = happyShift action_61
action_108 _ = happyReduce_51

action_109 (65) = happyShift action_122
action_109 (68) = happyShift action_123
action_109 _ = happyFail

action_110 (43) = happyShift action_42
action_110 (53) = happyShift action_43
action_110 (54) = happyShift action_44
action_110 (56) = happyShift action_45
action_110 (60) = happyShift action_46
action_110 (61) = happyShift action_121
action_110 (64) = happyShift action_47
action_110 (67) = happyShift action_48
action_110 (69) = happyShift action_49
action_110 (70) = happyShift action_50
action_110 (71) = happyShift action_51
action_110 (72) = happyShift action_52
action_110 (73) = happyShift action_53
action_110 (74) = happyShift action_54
action_110 (75) = happyShift action_55
action_110 (76) = happyShift action_56
action_110 (77) = happyShift action_57
action_110 (78) = happyShift action_58
action_110 (79) = happyShift action_59
action_110 (80) = happyShift action_60
action_110 (81) = happyShift action_61
action_110 (82) = happyShift action_62
action_110 (83) = happyShift action_63
action_110 (96) = happyShift action_64
action_110 (97) = happyShift action_65
action_110 _ = happyFail

action_111 (43) = happyShift action_42
action_111 (60) = happyShift action_46
action_111 (64) = happyShift action_47
action_111 (67) = happyShift action_48
action_111 (69) = happyShift action_49
action_111 (71) = happyShift action_51
action_111 (72) = happyShift action_52
action_111 (73) = happyShift action_53
action_111 (74) = happyShift action_54
action_111 (75) = happyShift action_55
action_111 (76) = happyShift action_56
action_111 (77) = happyShift action_57
action_111 (78) = happyShift action_58
action_111 (79) = happyShift action_59
action_111 (80) = happyShift action_60
action_111 (81) = happyShift action_61
action_111 (82) = happyShift action_62
action_111 (83) = happyShift action_63
action_111 (97) = happyShift action_65
action_111 _ = happyReduce_39

action_112 (43) = happyShift action_42
action_112 (53) = happyShift action_43
action_112 (56) = happyShift action_45
action_112 (60) = happyShift action_46
action_112 (64) = happyShift action_47
action_112 (67) = happyShift action_48
action_112 (69) = happyShift action_49
action_112 (71) = happyShift action_51
action_112 (72) = happyShift action_52
action_112 (73) = happyShift action_53
action_112 (74) = happyShift action_54
action_112 (75) = happyShift action_55
action_112 (76) = happyShift action_56
action_112 (77) = happyShift action_57
action_112 (78) = happyShift action_58
action_112 (79) = happyShift action_59
action_112 (80) = happyShift action_60
action_112 (81) = happyShift action_61
action_112 (82) = happyShift action_62
action_112 (83) = happyShift action_63
action_112 (96) = happyShift action_64
action_112 (97) = happyShift action_65
action_112 _ = happyReduce_38

action_113 (43) = happyShift action_42
action_113 (56) = happyShift action_45
action_113 (60) = happyShift action_46
action_113 (64) = happyShift action_47
action_113 (67) = happyShift action_48
action_113 (69) = happyShift action_49
action_113 (71) = happyShift action_51
action_113 (72) = happyShift action_52
action_113 (73) = happyShift action_53
action_113 (74) = happyShift action_54
action_113 (75) = happyShift action_55
action_113 (76) = happyShift action_56
action_113 (77) = happyShift action_57
action_113 (78) = happyShift action_58
action_113 (79) = happyShift action_59
action_113 (80) = happyShift action_60
action_113 (81) = happyShift action_61
action_113 (82) = happyShift action_62
action_113 (83) = happyShift action_63
action_113 (96) = happyShift action_64
action_113 (97) = happyShift action_65
action_113 _ = happyReduce_37

action_114 _ = happyReduce_31

action_115 _ = happyReduce_32

action_116 (60) = happyShift action_120
action_116 _ = happyReduce_10

action_117 (27) = happyShift action_20
action_117 (28) = happyShift action_21
action_117 (29) = happyShift action_22
action_117 (30) = happyShift action_23
action_117 (31) = happyShift action_24
action_117 (55) = happyShift action_25
action_117 (61) = happyShift action_119
action_117 (64) = happyShift action_26
action_117 (66) = happyShift action_27
action_117 (77) = happyShift action_28
action_117 (78) = happyShift action_29
action_117 (98) = happyShift action_30
action_117 (99) = happyShift action_31
action_117 (14) = happyGoto action_118
action_117 _ = happyFail

action_118 (43) = happyShift action_42
action_118 (53) = happyShift action_43
action_118 (54) = happyShift action_44
action_118 (56) = happyShift action_45
action_118 (60) = happyShift action_46
action_118 (61) = happyShift action_137
action_118 (64) = happyShift action_47
action_118 (67) = happyShift action_48
action_118 (69) = happyShift action_49
action_118 (70) = happyShift action_50
action_118 (71) = happyShift action_51
action_118 (72) = happyShift action_52
action_118 (73) = happyShift action_53
action_118 (74) = happyShift action_54
action_118 (75) = happyShift action_55
action_118 (76) = happyShift action_56
action_118 (77) = happyShift action_57
action_118 (78) = happyShift action_58
action_118 (79) = happyShift action_59
action_118 (80) = happyShift action_60
action_118 (81) = happyShift action_61
action_118 (82) = happyShift action_62
action_118 (83) = happyShift action_63
action_118 (96) = happyShift action_64
action_118 (97) = happyShift action_65
action_118 _ = happyFail

action_119 _ = happyReduce_12

action_120 (27) = happyShift action_20
action_120 (28) = happyShift action_21
action_120 (29) = happyShift action_22
action_120 (30) = happyShift action_23
action_120 (31) = happyShift action_24
action_120 (55) = happyShift action_25
action_120 (61) = happyShift action_136
action_120 (64) = happyShift action_26
action_120 (66) = happyShift action_27
action_120 (77) = happyShift action_28
action_120 (78) = happyShift action_29
action_120 (98) = happyShift action_30
action_120 (99) = happyShift action_31
action_120 (14) = happyGoto action_135
action_120 _ = happyFail

action_121 _ = happyReduce_60

action_122 _ = happyReduce_29

action_123 (27) = happyShift action_20
action_123 (28) = happyShift action_21
action_123 (29) = happyShift action_22
action_123 (30) = happyShift action_23
action_123 (31) = happyShift action_24
action_123 (55) = happyShift action_25
action_123 (64) = happyShift action_26
action_123 (66) = happyShift action_27
action_123 (77) = happyShift action_28
action_123 (78) = happyShift action_29
action_123 (98) = happyShift action_30
action_123 (99) = happyShift action_31
action_123 (14) = happyGoto action_134
action_123 _ = happyFail

action_124 (27) = happyShift action_20
action_124 (28) = happyShift action_21
action_124 (29) = happyShift action_22
action_124 (30) = happyShift action_23
action_124 (31) = happyShift action_24
action_124 (55) = happyShift action_25
action_124 (64) = happyShift action_26
action_124 (66) = happyShift action_27
action_124 (77) = happyShift action_28
action_124 (78) = happyShift action_29
action_124 (98) = happyShift action_30
action_124 (99) = happyShift action_31
action_124 (14) = happyGoto action_133
action_124 _ = happyFail

action_125 _ = happyReduce_16

action_126 _ = happyReduce_72

action_127 (30) = happyShift action_34
action_127 (9) = happyGoto action_132
action_127 _ = happyFail

action_128 (30) = happyShift action_131
action_128 _ = happyFail

action_129 (35) = happyShift action_74
action_129 (37) = happyShift action_75
action_129 (38) = happyShift action_76
action_129 (50) = happyShift action_77
action_129 (51) = happyShift action_78
action_129 (52) = happyShift action_79
action_129 (63) = happyShift action_130
action_129 (12) = happyGoto action_89
action_129 (13) = happyGoto action_68
action_129 (16) = happyGoto action_69
action_129 (17) = happyGoto action_70
action_129 (18) = happyGoto action_71
action_129 (25) = happyGoto action_72
action_129 (26) = happyGoto action_73
action_129 _ = happyFail

action_130 _ = happyReduce_4

action_131 (57) = happyShift action_13
action_131 (24) = happyGoto action_139
action_131 _ = happyReduce_75

action_132 _ = happyReduce_23

action_133 (43) = happyShift action_42
action_133 (53) = happyShift action_43
action_133 (54) = happyShift action_44
action_133 (56) = happyShift action_45
action_133 (60) = happyShift action_46
action_133 (64) = happyShift action_47
action_133 (67) = happyShift action_48
action_133 (69) = happyShift action_49
action_133 (71) = happyShift action_51
action_133 (72) = happyShift action_52
action_133 (73) = happyShift action_53
action_133 (74) = happyShift action_54
action_133 (75) = happyShift action_55
action_133 (76) = happyShift action_56
action_133 (77) = happyShift action_57
action_133 (78) = happyShift action_58
action_133 (79) = happyShift action_59
action_133 (80) = happyShift action_60
action_133 (81) = happyShift action_61
action_133 (82) = happyShift action_62
action_133 (83) = happyShift action_63
action_133 (96) = happyShift action_64
action_133 (97) = happyShift action_65
action_133 _ = happyReduce_53

action_134 (43) = happyShift action_42
action_134 (53) = happyShift action_43
action_134 (54) = happyShift action_44
action_134 (56) = happyShift action_45
action_134 (60) = happyShift action_46
action_134 (64) = happyShift action_47
action_134 (67) = happyShift action_48
action_134 (69) = happyShift action_49
action_134 (70) = happyShift action_50
action_134 (71) = happyShift action_51
action_134 (72) = happyShift action_52
action_134 (73) = happyShift action_53
action_134 (74) = happyShift action_54
action_134 (75) = happyShift action_55
action_134 (76) = happyShift action_56
action_134 (77) = happyShift action_57
action_134 (78) = happyShift action_58
action_134 (79) = happyShift action_59
action_134 (80) = happyShift action_60
action_134 (81) = happyShift action_61
action_134 (82) = happyShift action_62
action_134 (83) = happyShift action_63
action_134 (96) = happyShift action_64
action_134 (97) = happyShift action_65
action_134 _ = happyReduce_61

action_135 (43) = happyShift action_42
action_135 (53) = happyShift action_43
action_135 (54) = happyShift action_44
action_135 (56) = happyShift action_45
action_135 (60) = happyShift action_46
action_135 (61) = happyShift action_138
action_135 (64) = happyShift action_47
action_135 (67) = happyShift action_48
action_135 (69) = happyShift action_49
action_135 (70) = happyShift action_50
action_135 (71) = happyShift action_51
action_135 (72) = happyShift action_52
action_135 (73) = happyShift action_53
action_135 (74) = happyShift action_54
action_135 (75) = happyShift action_55
action_135 (76) = happyShift action_56
action_135 (77) = happyShift action_57
action_135 (78) = happyShift action_58
action_135 (79) = happyShift action_59
action_135 (80) = happyShift action_60
action_135 (81) = happyShift action_61
action_135 (82) = happyShift action_62
action_135 (83) = happyShift action_63
action_135 (96) = happyShift action_64
action_135 (97) = happyShift action_65
action_135 _ = happyFail

action_136 _ = happyReduce_14

action_137 _ = happyReduce_11

action_138 _ = happyReduce_13

action_139 _ = happyReduce_73

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 ([happy_var_1]
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_2  4 happyReduction_2
happyReduction_2 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  5 happyReduction_3
happyReduction_3 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happyReduce 10 6 happyReduction_4
happyReduction_4 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_9) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (L _ LId happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (Function happy_var_2 happy_var_4 happy_var_7 happy_var_9
	) `HappyStk` happyRest

happyReduce_5 = happyReduce 8 6 happyReduction_5
happyReduction_5 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (L _ LId happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (Function happy_var_2 happy_var_4 (Type "void") happy_var_7
	) `HappyStk` happyRest

happyReduce_6 = happySpecReduce_3  7 happyReduction_6
happyReduction_6 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  7 happyReduction_7
happyReduction_7 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 ([happy_var_1]
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happyReduce 4 8 happyReduction_8
happyReduction_8 ((HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn24  happy_var_2) `HappyStk`
	(HappyTerminal (L _ LId happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 ((happy_var_1, happy_var_2, happy_var_4)
	) `HappyStk` happyRest

happyReduce_9 = happySpecReduce_1  9 happyReduction_9
happyReduction_9 (HappyTerminal (L _ LId happy_var_1))
	 =  HappyAbsSyn9
		 (Type happy_var_1
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_2  9 happyReduction_10
happyReduction_10 (HappyAbsSyn10  happy_var_2)
	(HappyTerminal (L _ LId happy_var_1))
	 =  HappyAbsSyn9
		 (ArrayOf happy_var_1 happy_var_2
	)
happyReduction_10 _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  10 happyReduction_11
happyReduction_11 _
	(HappyAbsSyn14  happy_var_2)
	_
	 =  HappyAbsSyn10
		 ([Just happy_var_2]
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_2  10 happyReduction_12
happyReduction_12 _
	_
	 =  HappyAbsSyn10
		 ([Nothing]
	)

happyReduce_13 = happyReduce 4 10 happyReduction_13
happyReduction_13 (_ `HappyStk`
	(HappyAbsSyn14  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (happy_var_1 ++ [Just happy_var_3]
	) `HappyStk` happyRest

happyReduce_14 = happySpecReduce_3  10 happyReduction_14
happyReduction_14 _
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1 ++ [Nothing]
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_2  11 happyReduction_15
happyReduction_15 _
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 ([happy_var_1]
	)
happyReduction_15 _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  11 happyReduction_16
happyReduction_16 _
	(HappyAbsSyn12  happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  12 happyReduction_17
happyReduction_17 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  12 happyReduction_18
happyReduction_18 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  12 happyReduction_19
happyReduction_19 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  12 happyReduction_20
happyReduction_20 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  12 happyReduction_21
happyReduction_21 (HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  12 happyReduction_22
happyReduction_22 (HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happyReduce 4 13 happyReduction_23
happyReduction_23 ((HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn23  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (VarDeclaration VarKind happy_var_4 happy_var_2
	) `HappyStk` happyRest

happyReduce_24 = happySpecReduce_1  14 happyReduction_24
happyReduction_24 (HappyTerminal (L _ LChar happy_var_1))
	 =  HappyAbsSyn14
		 (Char happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  14 happyReduction_25
happyReduction_25 (HappyTerminal (L _ LNum happy_var_1))
	 =  HappyAbsSyn14
		 (Number happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  14 happyReduction_26
happyReduction_26 (HappyTerminal (L _ LBool happy_var_1))
	 =  HappyAbsSyn14
		 (Bool happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  14 happyReduction_27
happyReduction_27 (HappyTerminal (L _ LString happy_var_1))
	 =  HappyAbsSyn14
		 (Str happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  14 happyReduction_28
happyReduction_28 (HappyTerminal (L _ LId happy_var_1))
	 =  HappyAbsSyn14
		 (Var happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happyReduce 4 14 happyReduction_29
happyReduction_29 (_ `HappyStk`
	(HappyAbsSyn15  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (FunctionCall happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_30 = happySpecReduce_3  14 happyReduction_30
happyReduction_30 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (B "." happy_var_1 happy_var_3
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  14 happyReduction_31
happyReduction_31 (HappyTerminal (L _ LId happy_var_3))
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (TypeCast happy_var_1 happy_var_3
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  14 happyReduction_32
happyReduction_32 _
	(HappyAbsSyn14  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (happy_var_2
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_3  14 happyReduction_33
happyReduction_33 (HappyAbsSyn14  happy_var_3)
	(HappyTerminal (L _ LPlus happy_var_2))
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (B happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  14 happyReduction_34
happyReduction_34 (HappyAbsSyn14  happy_var_3)
	(HappyTerminal (L _ LMinus happy_var_2))
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (B happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  14 happyReduction_35
happyReduction_35 (HappyAbsSyn14  happy_var_3)
	(HappyTerminal (L _ LMul happy_var_2))
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (B happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  14 happyReduction_36
happyReduction_36 (HappyAbsSyn14  happy_var_3)
	(HappyTerminal (L _ LDiv happy_var_2))
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (B happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  14 happyReduction_37
happyReduction_37 (HappyAbsSyn14  happy_var_3)
	(HappyTerminal (L _ LAnd happy_var_2))
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (B happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  14 happyReduction_38
happyReduction_38 (HappyAbsSyn14  happy_var_3)
	(HappyTerminal (L _ LOr happy_var_2))
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (B happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_3  14 happyReduction_39
happyReduction_39 (HappyAbsSyn14  happy_var_3)
	(HappyTerminal (L _ LXor happy_var_2))
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (B happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_3  14 happyReduction_40
happyReduction_40 (HappyAbsSyn14  happy_var_3)
	(HappyTerminal (L _ LEquals happy_var_2))
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (B happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_3  14 happyReduction_41
happyReduction_41 (HappyAbsSyn14  happy_var_3)
	(HappyTerminal (L _ LNotEquals happy_var_2))
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (B happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_41 _ _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_3  14 happyReduction_42
happyReduction_42 (HappyAbsSyn14  happy_var_3)
	(HappyTerminal (L _ LGreater happy_var_2))
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (B happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_42 _ _ _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_3  14 happyReduction_43
happyReduction_43 (HappyAbsSyn14  happy_var_3)
	(HappyTerminal (L _ LGreaterEqual happy_var_2))
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (B happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_3  14 happyReduction_44
happyReduction_44 (HappyAbsSyn14  happy_var_3)
	(HappyTerminal (L _ LLess happy_var_2))
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (B happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_3  14 happyReduction_45
happyReduction_45 (HappyAbsSyn14  happy_var_3)
	(HappyTerminal (L _ LLessEqual happy_var_2))
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (B happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_45 _ _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_3  14 happyReduction_46
happyReduction_46 (HappyAbsSyn14  happy_var_3)
	(HappyTerminal (L _ LExp happy_var_2))
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (B happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_46 _ _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_3  14 happyReduction_47
happyReduction_47 (HappyAbsSyn14  happy_var_3)
	(HappyTerminal (L _ LRShift happy_var_2))
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (B happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_47 _ _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_3  14 happyReduction_48
happyReduction_48 (HappyAbsSyn14  happy_var_3)
	(HappyTerminal (L _ LLShift happy_var_2))
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (B happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_48 _ _ _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_3  14 happyReduction_49
happyReduction_49 (HappyAbsSyn14  happy_var_3)
	(HappyTerminal (L _ LBOr happy_var_2))
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (B happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_49 _ _ _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_3  14 happyReduction_50
happyReduction_50 (HappyAbsSyn14  happy_var_3)
	(HappyTerminal (L _ LBAnd happy_var_2))
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (B happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_50 _ _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_3  14 happyReduction_51
happyReduction_51 (HappyAbsSyn14  happy_var_3)
	(HappyTerminal (L _ LPercentage happy_var_2))
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (B happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_51 _ _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_3  14 happyReduction_52
happyReduction_52 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (R happy_var_1 happy_var_3 (Number "1")
	)
happyReduction_52 _ _ _  = notHappyAtAll 

happyReduce_53 = happyReduce 5 14 happyReduction_53
happyReduction_53 ((HappyAbsSyn14  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (R happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_54 = happySpecReduce_2  14 happyReduction_54
happyReduction_54 (HappyAbsSyn14  happy_var_2)
	(HappyTerminal (L _ LHash happy_var_1))
	 =  HappyAbsSyn14
		 (U happy_var_1 happy_var_2
	)
happyReduction_54 _ _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_2  14 happyReduction_55
happyReduction_55 (HappyAbsSyn14  happy_var_2)
	(HappyTerminal (L _  LAt happy_var_1))
	 =  HappyAbsSyn14
		 (U happy_var_1 happy_var_2
	)
happyReduction_55 _ _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_2  14 happyReduction_56
happyReduction_56 (HappyAbsSyn14  happy_var_2)
	(HappyTerminal (L _ LBNot happy_var_1))
	 =  HappyAbsSyn14
		 (U happy_var_1 happy_var_2
	)
happyReduction_56 _ _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_2  14 happyReduction_57
happyReduction_57 (HappyAbsSyn14  happy_var_2)
	(HappyTerminal (L _ LMinus happy_var_1))
	 =  HappyAbsSyn14
		 (U happy_var_1 happy_var_2
	)
happyReduction_57 _ _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_2  14 happyReduction_58
happyReduction_58 (HappyAbsSyn14  happy_var_2)
	(HappyTerminal (L _ LPlus happy_var_1))
	 =  HappyAbsSyn14
		 (U happy_var_1 happy_var_2
	)
happyReduction_58 _ _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_2  14 happyReduction_59
happyReduction_59 (HappyAbsSyn14  happy_var_2)
	(HappyTerminal (L _ LNot happy_var_1))
	 =  HappyAbsSyn14
		 (U happy_var_1 happy_var_2
	)
happyReduction_59 _ _  = notHappyAtAll 

happyReduce_60 = happyReduce 4 14 happyReduction_60
happyReduction_60 (_ `HappyStk`
	(HappyAbsSyn14  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (B "[]" happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_61 = happySpecReduce_3  15 happyReduction_61
happyReduction_61 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_61 _ _ _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_1  15 happyReduction_62
happyReduction_62 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn15
		 ([happy_var_1]
	)
happyReduction_62 _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_2  16 happyReduction_63
happyReduction_63 (HappyAbsSyn14  happy_var_2)
	_
	 =  HappyAbsSyn16
		 (Return happy_var_2
	)
happyReduction_63 _ _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_1  17 happyReduction_64
happyReduction_64 _
	 =  HappyAbsSyn17
		 (Continue
	)

happyReduce_65 = happySpecReduce_1  18 happyReduction_65
happyReduction_65 _
	 =  HappyAbsSyn18
		 (Break
	)

happyReduce_66 = happyReduce 6 19 happyReduction_66
happyReduction_66 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (L _ LId happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 ((happy_var_2, happy_var_4)
	) `HappyStk` happyRest

happyReduce_67 = happySpecReduce_1  20 happyReduction_67
happyReduction_67 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn20
		 ([happy_var_1]
	)
happyReduction_67 _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_3  20 happyReduction_68
happyReduction_68 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_68 _ _ _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_3  21 happyReduction_69
happyReduction_69 (HappyTerminal (L _ LId happy_var_3))
	_
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn21
		 ((happy_var_1, happy_var_3)
	)
happyReduction_69 _ _ _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_1  22 happyReduction_70
happyReduction_70 (HappyTerminal (L _ LId happy_var_1))
	 =  HappyAbsSyn22
		 ([happy_var_1]
	)
happyReduction_70 _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_3  22 happyReduction_71
happyReduction_71 (HappyTerminal (L _ LId happy_var_3))
	_
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_71 _ _ _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_2  23 happyReduction_72
happyReduction_72 (HappyAbsSyn24  happy_var_2)
	(HappyTerminal (L _ LId happy_var_1))
	 =  HappyAbsSyn23
		 ([(happy_var_1, happy_var_2)]
	)
happyReduction_72 _ _  = notHappyAtAll 

happyReduce_73 = happyReduce 4 23 happyReduction_73
happyReduction_73 ((HappyAbsSyn24  happy_var_4) `HappyStk`
	(HappyTerminal (L _ LId happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn23  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn23
		 (happy_var_1 ++ [(happy_var_3, happy_var_4)]
	) `HappyStk` happyRest

happyReduce_74 = happySpecReduce_2  24 happyReduction_74
happyReduction_74 (HappyAbsSyn14  happy_var_2)
	_
	 =  HappyAbsSyn24
		 (Just happy_var_2
	)
happyReduction_74 _ _  = notHappyAtAll 

happyReduce_75 = happySpecReduce_0  24 happyReduction_75
happyReduction_75  =  HappyAbsSyn24
		 (Nothing
	)

happyReduce_76 = happySpecReduce_2  25 happyReduction_76
happyReduction_76 (HappyAbsSyn15  happy_var_2)
	_
	 =  HappyAbsSyn25
		 (Print happy_var_2
	)
happyReduction_76 _ _  = notHappyAtAll 

happyReduce_77 = happySpecReduce_2  26 happyReduction_77
happyReduction_77 (HappyAbsSyn14  happy_var_2)
	_
	 =  HappyAbsSyn26
		 (Grab happy_var_2
	)
happyReduction_77 _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 100 100 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	L _ LNum happy_dollar_dollar -> cont 27;
	L _ LChar happy_dollar_dollar -> cont 28;
	L _ LBool happy_dollar_dollar -> cont 29;
	L _ LId happy_dollar_dollar -> cont 30;
	L _ LString happy_dollar_dollar -> cont 31;
	L _ LConst s -> cont 32;
	L _ LStatic s -> cont 33;
	L _ LFn s -> cont 34;
	L _ LVar s -> cont 35;
	L _ LBy s -> cont 36;
	L _ LPrint s -> cont 37;
	L _ LGrab s -> cont 38;
	L _ LStruct s -> cont 39;
	L _ LUnion s -> cont 40;
	L _ LEnum s -> cont 41;
	L _ LUnsigned s -> cont 42;
	L _ LAs s -> cont 43;
	L _ LType s -> cont 44;
	L _ LIf s -> cont 45;
	L _ LElse s -> cont 46;
	L _ LLoop s -> cont 47;
	L _ LFor s -> cont 48;
	L _ LWhile s -> cont 49;
	L _ LBreak s -> cont 50;
	L _ LContinue s -> cont 51;
	L _ LReturn s -> cont 52;
	L _ LAnd happy_dollar_dollar -> cont 53;
	L _ LOr happy_dollar_dollar -> cont 54;
	L _ LNot happy_dollar_dollar -> cont 55;
	L _ LXor happy_dollar_dollar -> cont 56;
	L _ LEqual s -> cont 57;
	L _ LColon s -> cont 58;
	L _ LSemicolon s -> cont 59;
	L _ LOpenBracket s -> cont 60;
	L _ LCloseBracket s -> cont 61;
	L _ LOpenCurly s -> cont 62;
	L _ LCloseCurly s -> cont 63;
	L _ LOpenParenthesis s -> cont 64;
	L _ LCloseParenthesis s -> cont 65;
	L _ LHash happy_dollar_dollar -> cont 66;
	L _ LPercentage happy_dollar_dollar -> cont 67;
	L _ LComma s -> cont 68;
	L _ LDot s -> cont 69;
	L _ LDots s -> cont 70;
	L _ LEquals happy_dollar_dollar -> cont 71;
	L _ LNotEquals happy_dollar_dollar -> cont 72;
	L _ LGreater happy_dollar_dollar -> cont 73;
	L _ LGreaterEqual happy_dollar_dollar -> cont 74;
	L _ LLess happy_dollar_dollar -> cont 75;
	L _ LLessEqual happy_dollar_dollar -> cont 76;
	L _ LPlus happy_dollar_dollar -> cont 77;
	L _ LMinus happy_dollar_dollar -> cont 78;
	L _ LMul happy_dollar_dollar -> cont 79;
	L _ LDiv happy_dollar_dollar -> cont 80;
	L _ LExp happy_dollar_dollar -> cont 81;
	L _ LRShift happy_dollar_dollar -> cont 82;
	L _ LLShift happy_dollar_dollar -> cont 83;
	L _ LPlusEqual s -> cont 84;
	L _ LMinusEqual s -> cont 85;
	L _ LMulEqual s -> cont 86;
	L _ LDivEqual s -> cont 87;
	L _ LModEqual s -> cont 88;
	L _ LRShiftEqual s -> cont 89;
	L _ LLShiftEqual s -> cont 90;
	L _ LBAndEqual s -> cont 91;
	L _ LBOrEqual s -> cont 92;
	L _ LXorEqual s -> cont 93;
	L _ LAndEqual s -> cont 94;
	L _ LOrEqual s -> cont 95;
	L _ LBOr happy_dollar_dollar -> cont 96;
	L _ LBAnd happy_dollar_dollar -> cont 97;
	L _ LBNot happy_dollar_dollar -> cont 98;
	L _  LAt happy_dollar_dollar -> cont 99;
	_ -> happyError' (tk:tks)
	}

happyError_ 100 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Lexeme)] -> HappyIdentity a
happyError' = HappyIdentity . happyError

parser tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


data Type = Type String | ArrayOf String [Maybe Expr] deriving (Show)
data Global = Function String [(String, Maybe Expr, Type)] Type [Instruction]
             deriving (Show)
data Expr = B String Expr Expr
          | U String Expr
          | Char String
          | Number String
          | Bool String
          | Str String
          | Var String
          | FunctionCall Expr [Expr]
          | TypeCast Expr String
          | R Expr Expr Expr
             deriving (Show)

data Instruction = VarDeclaration VKind Type [(String, Maybe Expr)]
                 | Break | Continue | Return Expr
                 | Print [Expr] | Grab Expr deriving (Show)
data VKind = VarKind | Const | Static deriving (Show)
type Initialization = Maybe Expr
happyError x = error "Error gramatical?"
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 30 "templates/GenericTemplate.hs" #-}








{-# LINE 51 "templates/GenericTemplate.hs" #-}

{-# LINE 61 "templates/GenericTemplate.hs" #-}

{-# LINE 70 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	 (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 148 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 246 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--	trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 312 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.

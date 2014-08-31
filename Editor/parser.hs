{-# OPTIONS_GHC -w #-}
module Parser where

import Tokens
import Control.Applicative(Applicative(..))

-- parser produced by Happy Version 1.19.4

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19
	= HappyTerminal (Token)
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

action_0 (21) = happyShift action_5
action_0 (4) = happyGoto action_6
action_0 (5) = happyGoto action_2
action_0 (6) = happyGoto action_3
action_0 (7) = happyGoto action_4
action_0 _ = happyReduce_2

action_1 (21) = happyShift action_5
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 (7) = happyGoto action_4
action_1 _ = happyFail

action_2 _ = happyReduce_1

action_3 (21) = happyShift action_5
action_3 (7) = happyGoto action_11
action_3 _ = happyReduce_3

action_4 _ = happyReduce_4

action_5 (21) = happyShift action_10
action_5 (8) = happyGoto action_7
action_5 (9) = happyGoto action_8
action_5 (10) = happyGoto action_9
action_5 _ = happyReduce_7

action_6 (32) = happyAccept
action_6 _ = happyFail

action_7 (31) = happyShift action_13
action_7 _ = happyFail

action_8 (21) = happyShift action_10
action_8 (10) = happyGoto action_12
action_8 _ = happyReduce_8

action_9 _ = happyReduce_9

action_10 _ = happyReduce_11

action_11 _ = happyReduce_5

action_12 _ = happyReduce_10

action_13 (22) = happyShift action_17
action_13 (11) = happyGoto action_14
action_13 (12) = happyGoto action_15
action_13 (13) = happyGoto action_16
action_13 _ = happyReduce_12

action_14 (29) = happyShift action_19
action_14 _ = happyFail

action_15 (22) = happyShift action_17
action_15 (13) = happyGoto action_18
action_15 _ = happyReduce_13

action_16 _ = happyReduce_14

action_17 _ = happyReduce_16

action_18 _ = happyReduce_15

action_19 (30) = happyReduce_17
action_19 (14) = happyGoto action_20
action_19 (15) = happyGoto action_21
action_19 (16) = happyGoto action_22
action_19 (17) = happyGoto action_23
action_19 _ = happyReduce_22

action_20 (30) = happyShift action_29
action_20 _ = happyFail

action_21 (30) = happyReduce_18
action_21 (16) = happyGoto action_28
action_21 (17) = happyGoto action_23
action_21 _ = happyReduce_22

action_22 _ = happyReduce_19

action_23 (20) = happyShift action_26
action_23 (21) = happyShift action_27
action_23 (18) = happyGoto action_24
action_23 (19) = happyGoto action_25
action_23 _ = happyFail

action_24 (26) = happyShift action_35
action_24 _ = happyFail

action_25 (27) = happyShift action_34
action_25 _ = happyFail

action_26 _ = happyReduce_24

action_27 (21) = happyShift action_30
action_27 (24) = happyShift action_31
action_27 (25) = happyShift action_32
action_27 (28) = happyShift action_33
action_27 _ = happyReduce_25

action_28 _ = happyReduce_20

action_29 _ = happyReduce_6

action_30 (28) = happyShift action_40
action_30 _ = happyFail

action_31 (22) = happyShift action_39
action_31 _ = happyFail

action_32 (22) = happyShift action_38
action_32 _ = happyFail

action_33 (22) = happyShift action_36
action_33 (23) = happyShift action_37
action_33 _ = happyFail

action_34 _ = happyReduce_21

action_35 _ = happyReduce_23

action_36 _ = happyReduce_28

action_37 (22) = happyShift action_42
action_37 _ = happyFail

action_38 _ = happyReduce_27

action_39 _ = happyReduce_26

action_40 (23) = happyShift action_41
action_40 _ = happyFail

action_41 (22) = happyShift action_44
action_41 _ = happyFail

action_42 (22) = happyShift action_43
action_42 _ = happyReduce_32

action_43 _ = happyReduce_31

action_44 (22) = happyShift action_45
action_44 _ = happyReduce_30

action_45 _ = happyReduce_29

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_0  5 happyReduction_2
happyReduction_2  =  HappyAbsSyn5
		 ([]
	)

happyReduce_3 = happySpecReduce_1  5 happyReduction_3
happyReduction_3 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  6 happyReduction_4
happyReduction_4 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 ([happy_var_1]
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_2  6 happyReduction_5
happyReduction_5 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_5 _ _  = notHappyAtAll 

happyReduce_6 = happyReduce 7 7 happyReduction_6
happyReduction_6 (_ `HappyStk`
	(HappyAbsSyn14  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	(HappyTerminal (TKName _ happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (Procedure happy_var_1 happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_7 = happySpecReduce_0  8 happyReduction_7
happyReduction_7  =  HappyAbsSyn8
		 ([]
	)

happyReduce_8 = happySpecReduce_1  8 happyReduction_8
happyReduction_8 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  9 happyReduction_9
happyReduction_9 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 ([happy_var_1]
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_2  9 happyReduction_10
happyReduction_10 (HappyAbsSyn10  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_10 _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  10 happyReduction_11
happyReduction_11 (HappyTerminal (TKName _ happy_var_1))
	 =  HappyAbsSyn10
		 (Param happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_0  11 happyReduction_12
happyReduction_12  =  HappyAbsSyn11
		 ([]
	)

happyReduce_13 = happySpecReduce_1  11 happyReduction_13
happyReduction_13 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  12 happyReduction_14
happyReduction_14 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn12
		 ([happy_var_1]
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_2  12 happyReduction_15
happyReduction_15 (HappyAbsSyn13  happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_15 _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  13 happyReduction_16
happyReduction_16 (HappyTerminal (TKString _ happy_var_1))
	 =  HappyAbsSyn13
		 (Result happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_0  14 happyReduction_17
happyReduction_17  =  HappyAbsSyn14
		 ([]
	)

happyReduce_18 = happySpecReduce_1  14 happyReduction_18
happyReduction_18 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  15 happyReduction_19
happyReduction_19 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn15
		 ([happy_var_1]
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_2  15 happyReduction_20
happyReduction_20 (HappyAbsSyn16  happy_var_2)
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_20 _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  16 happyReduction_21
happyReduction_21 _
	(HappyAbsSyn19  happy_var_2)
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn16
		 (Cmd happy_var_1 happy_var_2
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_0  17 happyReduction_22
happyReduction_22  =  HappyAbsSyn17
		 ([]
	)

happyReduce_23 = happySpecReduce_3  17 happyReduction_23
happyReduction_23 _
	(HappyAbsSyn18  happy_var_2)
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  18 happyReduction_24
happyReduction_24 _
	 =  HappyAbsSyn18
		 (Finally
	)

happyReduce_25 = happySpecReduce_1  18 happyReduction_25
happyReduction_25 (HappyTerminal (TKName _ happy_var_1))
	 =  HappyAbsSyn18
		 (Boolean happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  18 happyReduction_26
happyReduction_26 (HappyTerminal (TKString _ happy_var_3))
	_
	(HappyTerminal (TKName _ happy_var_1))
	 =  HappyAbsSyn18
		 (Equals happy_var_1 happy_var_3
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  18 happyReduction_27
happyReduction_27 (HappyTerminal (TKString _ happy_var_3))
	_
	(HappyTerminal (TKName _ happy_var_1))
	 =  HappyAbsSyn18
		 (NotEquals happy_var_1 happy_var_3
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  19 happyReduction_28
happyReduction_28 (HappyTerminal (TKString _ happy_var_3))
	_
	(HappyTerminal (TKName _ happy_var_1))
	 =  HappyAbsSyn19
		 (Assignment happy_var_1 happy_var_3
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happyReduce 6 19 happyReduction_29
happyReduction_29 ((HappyTerminal (TKString _ happy_var_6)) `HappyStk`
	(HappyTerminal (TKString _ happy_var_5)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TKName _ happy_var_2)) `HappyStk`
	(HappyTerminal (TKName _ happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (Execution happy_var_1 (StdOut happy_var_2) happy_var_5 (StdIn happy_var_6)
	) `HappyStk` happyRest

happyReduce_30 = happyReduce 5 19 happyReduction_30
happyReduction_30 ((HappyTerminal (TKString _ happy_var_5)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TKName _ happy_var_2)) `HappyStk`
	(HappyTerminal (TKName _ happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (Execution happy_var_1 (StdOut happy_var_2) happy_var_5 (StdInNil)
	) `HappyStk` happyRest

happyReduce_31 = happyReduce 5 19 happyReduction_31
happyReduction_31 ((HappyTerminal (TKString _ happy_var_5)) `HappyStk`
	(HappyTerminal (TKString _ happy_var_4)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TKName _ happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (Execution happy_var_1 (StdOutNil) happy_var_4 (StdIn happy_var_5)
	) `HappyStk` happyRest

happyReduce_32 = happyReduce 4 19 happyReduction_32
happyReduction_32 ((HappyTerminal (TKString _ happy_var_4)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TKName _ happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (Execution happy_var_1 (StdOutNil) happy_var_4 (StdInNil)
	) `HappyStk` happyRest

happyNewToken action sts stk [] =
	action 32 32 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TKFinally _ -> cont 20;
	TKName _ happy_dollar_dollar -> cont 21;
	TKString _ happy_dollar_dollar -> cont 22;
	TKExec _ -> cont 23;
	TKEquals _ -> cont 24;
	TKNotEquals _ -> cont 25;
	TKColon _ -> cont 26;
	TKSemicolon _ -> cont 27;
	TKAssignment _ -> cont 28;
	TKOpenBracket _ -> cont 29;
	TKCloseBracket _ -> cont 30;
	TKMinus _ -> cont 31;
	_ -> happyError' (tk:tks)
	}

happyError_ 32 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

happyThen :: () => E a -> (a -> E b) -> E b
happyThen = (thenE)
happyReturn :: () => a -> E a
happyReturn = (returnE)
happyThen1 m k tks = (thenE) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> E a
happyReturn1 = \a tks -> (returnE) a
happyError' :: () => [(Token)] -> E a
happyError' = parseError

parse tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


data E a = Ok a | Failed String
	deriving Show

thenE :: E a -> (a -> E b) -> E b
m `thenE` k = 
   case m of 
       Ok a -> k a
       Failed e -> Failed e

returnE :: a -> E a
returnE a = Ok a

failE :: String -> E a
failE err = Failed err

catchE :: E a -> (String -> E a) -> E a
catchE m k = 
   case m of
      Ok a -> Ok a
      Failed e -> k e


tok f p s = f p s

--parseError :: [Token] -> P a
parseError []     = failE ("Parse error - unexpected end of file")
parseError tokens = failE ("Parse error at row " ++ (show ((tokenGetRow (tokens!!0))+1)) ++ ", column " ++ (show ((tokenGetColumn (tokens!!0))+1)))


data Procedure =
	Procedure Name [Parameter] [Result] [GuardedCommand]
	deriving Show

data Parameter =
	Param Name
	deriving Show

data Result =
	Result String
	deriving Show

data GuardedCommand =
	Cmd [Guard] Command
	deriving Show

data Guard =
	Boolean Name			|
	Equals Name String		|
	NotEquals Name String	|
	Finally
	deriving Show

data Command =
	Assignment Name String  |
	Execution Name ExecOutput String ExecArgument
	deriving Show

data ExecOutput =
	StdOutNil |
	StdOut Name
	deriving Show

data ExecArgument =
	StdInNil |
	StdIn String
	deriving Show

type Name = String
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

























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
     let i = (case x of { HappyErrorToken (i) -> i }) in
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
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
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
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









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


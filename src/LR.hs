{-# LANGUAGE GADTs #-}
module LR (parse) where
import Rep
import LRRep
import Tokenize (Token(..), Pos)
import Control.Monad.State

type Tokens = [(Token, Pos, Pos)]
type Parser a = State Tokens a

peek :: Parser Token 
peek = do
    list <- get
    case list of
        [] -> pure TEnd
        ((x,p,q):xs) -> pure x

shift :: Parser ()
shift = modify f where f [] = []
                       f (x:xs) = xs

data Status a where
    S0 :: Status Empty
    S1 :: Status (Ckeyword a)
    S2 :: Status (Ckeyword a)
    S3 :: Status (Cstatement a)
    S4 :: Status (Ckeyword a)
    S5 :: Status (Cfile a)
    S6 :: Status (Cid (Ckeyword a))
    S7 :: Status (Cid (Ckeyword a))
    S8 :: Status (Cnames (Ckeyword a))
    S9 :: Status (Cname a)
    S10 :: Status (Cid a)
    S11 :: Status (Cstatement (Cfile a))
    S12 :: Status (Ckeyword (Cid (Ckeyword a)))
    S13 :: Status (Ckeyword a)
    S14 :: Status (Cstructbind a)
    S15 :: Status (Clambdabinders (Cid (Ckeyword a)))
    S16 :: Status (Ckeyword (Cid (Ckeyword a)))
    S17 :: Status (Ckeyword (Cid (Ckeyword a)))
    S18 :: Status (Clambdabinder a)
    S19 :: Status (Ckeyword (Cname a))
    S20 :: Status (Ckeyword (Cid a))
    S21 :: Status (Cnames (Ckeyword (Cid (Ckeyword a))))
    S22 :: Status (Clabbinds (Ckeyword a))
    S23 :: Status (Ckeyword (Ckeyword a))
    S24 :: Status (Cid a)
    S25 :: Status (Clabbind a)
    S26 :: Status (Cid (Cstructbind a))
    S27 :: Status (Ckeyword (Clambdabinders (Cid (Ckeyword a))))
    S28 :: Status (Ckeyword a)
    S29 :: Status (Cprim a)
    S30 :: Status (Cmonad a)
    S31 :: Status (Ckeyword a)
    S32 :: Status (Cdisj2 a)
    S33 :: Status (Cprefix a)
    S34 :: Status (Cconj a)
    S35 :: Status (Cdisj a)
    S36 :: Status (Cstring a)
    S37 :: Status (Ckeyword a)
    S38 :: Status (Cstructdef a)
    S39 :: Status (Cid a)
    S40 :: Status (Cadd a)
    S41 :: Status (Ccall a)
    S42 :: Status (Ckeyword a)
    S43 :: Status (Ckeyword a)
    S44 :: Status (Creal a)
    S45 :: Status (Ckeyword a)
    S46 :: Status (Ckeyword a)
    S47 :: Status (Cnormal (Ckeyword (Cid (Ckeyword a))))
    S48 :: Status (Ccomp a)
    S49 :: Status (Ckeyword a)
    S50 :: Status (Ccocase a)
    S51 :: Status (Cequals a)
    S52 :: Status (Ckeyword a)
    S53 :: Status (Clet a)
    S54 :: Status (Ckeyword a)
    S55 :: Status (Cmul a)
    S56 :: Status (Cnot a)
    S57 :: Status (Cinteger a)
    S58 :: Status (Cterm a)
    S59 :: Status (Ckeyword a)
    S60 :: Status (Cconj2 a)
    S61 :: Status (Cexp a)
    S62 :: Status (Clambda a)
    S63 :: Status (Ckeyword a)
    S64 :: Status (Cnormal (Ckeyword (Cid (Ckeyword a))))
    S65 :: Status (Clambdabinders (Clambdabinder a))
    S66 :: Status (Cnames (Ckeyword (Cname a)))
    S67 :: Status (Cid (Ckeyword (Cid a)))
    S68 :: Status (Ckeyword (Clabbinds (Ckeyword a)))
    S69 :: Status (Ckeyword (Clabbind a))
    S70 :: Status (Cnormal (Ckeyword (Clambdabinders (Cid (Ckeyword a)))))
    S71 :: Status (Cnormal (Ckeyword a))
    S72 :: Status (Clabtypes (Ckeyword a))
    S73 :: Status (Clabtype a)
    S74 :: Status (Ckeyword (Ckeyword a))
    S75 :: Status (Cid a)
    S76 :: Status (Ckeyword (Cdisj2 a))
    S77 :: Status (Ckeyword (Cprefix a))
    S78 :: Status (Ckeyword (Cprefix a))
    S79 :: Status (Ckeyword (Cconj a))
    S80 :: Status (Cnormal (Ckeyword a))
    S81 :: Status (Ckeyword (Cstructdef a))
    S82 :: Status (Cterm (Cstructdef a))
    S83 :: Status (Ckeyword (Cid a))
    S84 :: Status (Ckeyword (Cid a))
    S85 :: Status (Ckeyword (Cid a))
    S86 :: Status (Ckeyword (Cadd a))
    S87 :: Status (Ckeyword (Ccall a))
    S88 :: Status (Cstruct (Ccall a))
    S89 :: Status (Ckeyword (Ccall a))
    S90 :: Status (Ckeyword a)
    S91 :: Status (Ckeyword (Ccall a))
    S92 :: Status (Clabterms (Ckeyword a))
    S93 :: Status (Cid a)
    S94 :: Status (Ckeyword (Ckeyword a))
    S95 :: Status (Cterms (Ckeyword a))
    S96 :: Status (Ckeyword a)
    S97 :: Status (Clabterm a)
    S98 :: Status (Cterm a)
    S99 :: Status (Ccase a)
    S100 :: Status (Ccases (Ckeyword a))
    S101 :: Status (Cid a)
    S102 :: Status (Clambdabinders (Ckeyword a))
    S103 :: Status (Ckeyword (Ccomp a))
    S104 :: Status (Ckeyword (Ccomp a))
    S105 :: Status (Ckeyword (Ccomp a))
    S106 :: Status (Ckeyword (Ccomp a))
    S107 :: Status (Ckeyword (Ccomp a))
    S108 :: Status (Ckeyword (Ccomp a))
    S109 :: Status (Cid (Ckeyword a))
    S110 :: Status (Cnormal (Ckeyword a))
    S111 :: Status (Cbinders (Ckeyword a))
    S112 :: Status (Ckeyword a)
    S113 :: Status (Cid a)
    S114 :: Status (Ckeyword (Clet a))
    S115 :: Status (Cterms (Ckeyword a))
    S116 :: Status (Ckeyword (Ckeyword a))
    S117 :: Status (Ckeyword (Cmul a))
    S118 :: Status (Ckeyword (Cmul a))
    S119 :: Status (Ckeyword (Cnot a))
    S120 :: Status (Ckeyword (Cterm a))
    S121 :: Status (Ckeyword (Cterm a))
    S122 :: Status (Ckeyword (Cterm a))
    S123 :: Status (Cprefix (Ckeyword a))
    S124 :: Status (Cid a)
    S125 :: Status (Ckeyword (Cconj2 a))
    S126 :: Status (Cnot (Ckeyword a))
    S127 :: Status (Clabbinds (Ckeyword (Clabbind a)))
    S128 :: Status (Ckeyword (Clabtypes (Ckeyword a)))
    S129 :: Status (Ckeyword (Clabtype a))
    S130 :: Status (Ckeyword (Cid a))
    S131 :: Status (Cconj2 (Ckeyword (Cdisj2 a)))
    S132 :: Status (Cmul (Ckeyword (Cprefix a)))
    S133 :: Status (Cmul (Ckeyword (Cprefix a)))
    S134 :: Status (Cdisj2 (Ckeyword (Cconj a)))
    S135 :: Status (Ckeyword (Cnormal (Ckeyword a)))
    S136 :: Status (Cid (Ckeyword (Cstructdef a)))
    S137 :: Status (Ckeyword (Cterm (Cstructdef a)))
    S138 :: Status (Cid a)
    S139 :: Status (Ccocase (Ckeyword (Cid a)))
    S140 :: Status (Ccocase (Ckeyword (Cid a)))
    S141 :: Status (Ccocase (Ckeyword (Cid a)))
    S142 :: Status (Ccomp (Ckeyword (Cadd a)))
    S143 :: Status (Cnormal (Ckeyword (Ccall a)))
    S144 :: Status (Cprim (Cstruct (Ccall a)))
    S145 :: Status (Cexp (Ckeyword (Ccall a)))
    S146 :: Status (Clabterms (Ckeyword a))
    S147 :: Status (Ckeyword (Ckeyword a))
    S148 :: Status (Cterms (Ckeyword a))
    S149 :: Status (Ccases (Ckeyword (Ccall a)))
    S150 :: Status (Ckeyword (Clabterms (Ckeyword a)))
    S151 :: Status (Ckeyword (Clabterms (Ckeyword a)))
    S152 :: Status (Ckeyword (Cid a))
    S153 :: Status (Ckeyword (Cterms (Ckeyword a)))
    S154 :: Status (Ckeyword (Clabterm a))
    S155 :: Status (Ckeyword (Cterm a))
    S156 :: Status (Ckeyword (Ccase a))
    S157 :: Status (Ckeyword (Ccases (Ckeyword a)))
    S158 :: Status (Cbinders a)
    S159 :: Status (Cbinders0 (Cid a))
    S160 :: Status (Ckeyword (Clambdabinders (Ckeyword a)))
    S161 :: Status (Ccomp (Ckeyword (Ccomp a)))
    S162 :: Status (Ccomp (Ckeyword (Ccomp a)))
    S163 :: Status (Ccomp (Ckeyword (Ccomp a)))
    S164 :: Status (Ccomp (Ckeyword (Ccomp a)))
    S165 :: Status (Ccomp (Ckeyword (Ccomp a)))
    S166 :: Status (Ccomp (Ckeyword (Ccomp a)))
    S167 :: Status (Ckeyword (Cid (Ckeyword a)))
    S168 :: Status (Ckeyword (Cnormal (Ckeyword a)))
    S169 :: Status (Ckeyword (Cbinders (Ckeyword a)))
    S170 :: Status (Cid (Ckeyword a))
    S171 :: Status (Cbinders (Cid a))
    S172 :: Status (Cmonad (Ckeyword (Clet a)))
    S173 :: Status (Ckeyword (Cterms (Ckeyword a)))
    S174 :: Status (Ckeyword (Cterms (Ckeyword a)))
    S175 :: Status (Cadd (Ckeyword (Cmul a)))
    S176 :: Status (Cadd (Ckeyword (Cmul a)))
    S177 :: Status (Cconj (Ckeyword (Cnot a)))
    S178 :: Status (Cterm (Ckeyword (Cterm a)))
    S179 :: Status (Cnormal (Ckeyword (Cterm a)))
    S180 :: Status (Cnormal (Ckeyword (Cterm a)))
    S181 :: Status (Cdisj (Ckeyword (Cconj2 a)))
    S182 :: Status (Clabtypes (Ckeyword (Clabtype a)))
    S183 :: Status (Cnormal (Ckeyword (Cid a)))
    S184 :: Status (Cterm (Ckeyword (Cnormal (Ckeyword a))))
    S185 :: Status (Ckeyword (Cid (Ckeyword (Cstructdef a))))
    S186 :: Status (Cnormal (Ckeyword (Cterm (Cstructdef a))))
    S187 :: Status (Ckeyword (Ccocase (Ckeyword (Cid a))))
    S188 :: Status (Ckeyword (Ccocase (Ckeyword (Cid a))))
    S189 :: Status (Ckeyword (Cnormal (Ckeyword (Ccall a))))
    S190 :: Status (Ckeyword (Clabterms (Ckeyword a)))
    S191 :: Status (Ckeyword (Cterms (Ckeyword a)))
    S192 :: Status (Ckeyword (Ccases (Ckeyword (Ccall a))))
    S193 :: Status (Cnormal (Ckeyword (Clabterms (Ckeyword a))))
    S194 :: Status (Cnormal (Ckeyword (Cid a)))
    S195 :: Status (Cid a)
    S196 :: Status (Clabterms (Ckeyword (Clabterm a)))
    S197 :: Status (Cterms (Ckeyword (Cterm a)))
    S198 :: Status (Ccases (Ckeyword (Ccase a)))
    S199 :: Status (Ckeyword (Cbinders0 (Cid a)))
    S200 :: Status (Clambda (Ckeyword (Clambdabinders (Ckeyword a))))
    S201 :: Status (Cnormal (Ckeyword (Cid (Ckeyword a))))
    S202 :: Status (Clambda (Ckeyword (Cbinders (Ckeyword a))))
    S203 :: Status (Cbinders (Cid (Ckeyword a)))
    S204 :: Status (Cterm (Ckeyword (Cterms (Ckeyword a))))
    S205 :: Status (Ckeyword (Cterm (Ckeyword (Cnormal (Ckeyword a)))))
    S206 :: Status (Cnormal (Ckeyword (Cid (Ckeyword (Cstructdef a)))))
    S207 :: Status (Cmonad (Ckeyword (Ccocase (Ckeyword (Cid a)))))
    S208 :: Status (Cid a)
    S209 :: Status (Clet (Ckeyword (Ccocase (Ckeyword (Cid a)))))
    S210 :: Status (Ckeyword (Ckeyword (Cnormal (Ckeyword (Ccall a)))))
    S211 :: Status (Ckeyword (Cnormal (Ckeyword (Clabterms (Ckeyword a)))))
    S212 :: Status (Cnormal (Ckeyword (Cbinders0 (Cid a))))
    S213 :: Status (Ckeyword (Cnormal (Ckeyword (Cid (Ckeyword a)))))
    S214 :: Status (Ckeyword (Cterm (Ckeyword (Cterms (Ckeyword a)))))
    S215 :: Status (Cterm (Ckeyword (Cterm (Ckeyword (Cnormal (Ckeyword a))))))
    S216 :: Status (Ckeyword (Cnormal (Ckeyword (Cid (Ckeyword (Cstructdef a))))))
    S217 :: Status (Ccases (Ckeyword (Ckeyword (Cnormal (Ckeyword (Ccall a))))))
    S218 :: Status (Ckeyword (Ckeyword (Cnormal (Ckeyword (Cid (Ckeyword a))))))
    S219 :: Status (Ckeyword (Ckeyword (Cnormal (Ckeyword (Cid (Ckeyword (Cstructdef a)))))))
    S220 :: Status (Ckeyword (Ccases (Ckeyword (Ckeyword (Cnormal (Ckeyword (Ccall a)))))))
    S221 :: Status (Cnormal (Ckeyword (Ckeyword (Cnormal (Ckeyword (Cid (Ckeyword a)))))))
    S222 :: Status (Cnormal (Ckeyword (Ckeyword (Cnormal (Ckeyword (Cid (Ckeyword (Cstructdef a))))))))

parse :: Tokens -> (Maybe [Statement], Tokens)
parse = runState (run S0 SEmpty)

run :: Status a -> a -> Parser (Maybe [Statement])
run s stack = do
    c <- peek
    run' s stack c

run' :: Status a -> a -> Token -> Parser (Maybe [Statement])
run' S0 stack (TKeyword "import") = shift >> run S1 (Skeyword stack S0)
run' S0 stack (TKeyword "def") = shift >> run S2 (Skeyword stack S0)
run' S0 stack (TKeyword "export") = shift >> run S4 (Skeyword stack S0)
run' S1 stack (TId arg) = shift >> run S6 (Sid stack S1 arg)
run' S2 stack (TId arg) = shift >> run S7 (Sid stack S2 arg)
run' S3 stack (TKeyword "import") = case stack of
    (Sstatement stack s x0) ->
        gotoFile s (Sfile stack s (reduction1 x0))
run' S3 stack (TKeyword "def") = case stack of
    (Sstatement stack s x0) ->
        gotoFile s (Sfile stack s (reduction1 x0))
run' S3 stack (TKeyword "export") = case stack of
    (Sstatement stack s x0) ->
        gotoFile s (Sfile stack s (reduction1 x0))
run' S3 stack TEnd = case stack of
    (Sstatement stack s x0) ->
        gotoFile s (Sfile stack s (reduction1 x0))
run' S4 stack (TId arg) = shift >> run S10 (Sid stack S4 arg)
run' S5 stack (TKeyword "import") = shift >> run S1 (Skeyword stack S5)
run' S5 stack (TKeyword "def") = shift >> run S2 (Skeyword stack S5)
run' S5 stack (TKeyword "export") = shift >> run S4 (Skeyword stack S5)
run' S5 stack TEnd = case stack of
    (Sfile stack s x0) -> pure (reduction0 x0)
run' S6 stack (TKeyword "import") = case stack of
    (Sid (Skeyword stack s) _ x0) ->
        gotoStatement s (Sstatement stack s (reduction3 x0))
run' S6 stack (TKeyword "def") = case stack of
    (Sid (Skeyword stack s) _ x0) ->
        gotoStatement s (Sstatement stack s (reduction3 x0))
run' S6 stack (TKeyword "export") = case stack of
    (Sid (Skeyword stack s) _ x0) ->
        gotoStatement s (Sstatement stack s (reduction3 x0))
run' S6 stack TEnd = case stack of
    (Sid (Skeyword stack s) _ x0) ->
        gotoStatement s (Sstatement stack s (reduction3 x0))
run' S6 stack (TKeyword "using") = shift >> run S12 (Skeyword stack S6)
run' S7 stack (TKeyword "⟨") = shift >> run S13 (Skeyword stack S7)
run' S7 stack (TId arg) = gotoStructbind S7 (Sstructbind stack S7 reduction91)
run' S7 stack (TKeyword "=") = shift >> run S16 (Skeyword stack S7)
run' S7 stack (TKeyword ":") = shift >> run S17 (Skeyword stack S7)
run' S8 stack (TKeyword "import") = case stack of
    (Snames (Skeyword stack s) _ x0) ->
        gotoStatement s (Sstatement stack s (reduction5 x0))
run' S8 stack (TKeyword "def") = case stack of
    (Snames (Skeyword stack s) _ x0) ->
        gotoStatement s (Sstatement stack s (reduction5 x0))
run' S8 stack (TKeyword "export") = case stack of
    (Snames (Skeyword stack s) _ x0) ->
        gotoStatement s (Sstatement stack s (reduction5 x0))
run' S8 stack TEnd = case stack of
    (Snames (Skeyword stack s) _ x0) ->
        gotoStatement s (Sstatement stack s (reduction5 x0))
run' S9 stack (TKeyword "import") = case stack of
    (Sname stack s x0) ->
        gotoNames s (Snames stack s (reduction6 x0))
run' S9 stack (TKeyword "def") = case stack of
    (Sname stack s x0) ->
        gotoNames s (Snames stack s (reduction6 x0))
run' S9 stack (TKeyword "export") = case stack of
    (Sname stack s x0) ->
        gotoNames s (Snames stack s (reduction6 x0))
run' S9 stack TEnd = case stack of
    (Sname stack s x0) ->
        gotoNames s (Snames stack s (reduction6 x0))
run' S9 stack (TKeyword ",") = shift >> run S19 (Skeyword stack S9)
run' S10 stack (TKeyword "import") = case stack of
    (Sid stack s x0) ->
        gotoName s (Sname stack s (reduction8 x0))
run' S10 stack TEnd = case stack of
    (Sid stack s x0) ->
        gotoName s (Sname stack s (reduction8 x0))
run' S10 stack (TKeyword "def") = case stack of
    (Sid stack s x0) ->
        gotoName s (Sname stack s (reduction8 x0))
run' S10 stack (TKeyword "export") = case stack of
    (Sid stack s x0) ->
        gotoName s (Sname stack s (reduction8 x0))
run' S10 stack (TKeyword "⇒") = shift >> run S20 (Skeyword stack S10)
run' S10 stack (TKeyword ",") = case stack of
    (Sid stack s x0) ->
        gotoName s (Sname stack s (reduction8 x0))
run' S11 stack (TKeyword "import") = case stack of
    (Sstatement (Sfile stack s x0) _ x1) ->
        gotoFile s (Sfile stack s (reduction2 x0 x1))
run' S11 stack (TKeyword "def") = case stack of
    (Sstatement (Sfile stack s x0) _ x1) ->
        gotoFile s (Sfile stack s (reduction2 x0 x1))
run' S11 stack (TKeyword "export") = case stack of
    (Sstatement (Sfile stack s x0) _ x1) ->
        gotoFile s (Sfile stack s (reduction2 x0 x1))
run' S11 stack TEnd = case stack of
    (Sstatement (Sfile stack s x0) _ x1) ->
        gotoFile s (Sfile stack s (reduction2 x0 x1))
run' S12 stack (TId arg) = shift >> run S10 (Sid stack S12 arg)
run' S13 stack (TKeyword "⟩") = shift >> run S23 (Skeyword stack S13)
run' S13 stack (TId arg) = shift >> run S24 (Sid stack S13 arg)
run' S14 stack (TId arg) = shift >> run S26 (Sid stack S14 arg)
run' S15 stack (TKeyword "=") = shift >> run S27 (Skeyword stack S15)
run' S16 stack (TKeyword "-") = shift >> run S28 (Skeyword stack S16)
run' S16 stack (TKeyword "⋆") = shift >> run S45 (Skeyword stack S16)
run' S16 stack (TKeyword "⟨") = shift >> run S31 (Skeyword stack S16)
run' S16 stack (TKeyword "λ") = shift >> run S46 (Skeyword stack S16)
run' S16 stack (TKeyword "(") = shift >> run S49 (Skeyword stack S16)
run' S16 stack (TKeyword "∃") = shift >> run S52 (Skeyword stack S16)
run' S16 stack (TKeyword "[") = shift >> run S54 (Skeyword stack S16)
run' S16 stack (TInteger arg) = shift >> run S57 (Sinteger stack S16 arg)
run' S16 stack (TString arg) = shift >> run S36 (Sstring stack S16 arg)
run' S16 stack (TKeyword "⟦") = shift >> run S37 (Skeyword stack S16)
run' S16 stack (TKeyword "~") = shift >> run S59 (Skeyword stack S16)
run' S16 stack (TId arg) = shift >> run S39 (Sid stack S16 arg)
run' S16 stack (TKeyword "{") = shift >> run S42 (Skeyword stack S16)
run' S16 stack (TReal arg) = shift >> run S44 (Sreal stack S16 arg)
run' S16 stack (TKeyword "⟮") = shift >> run S43 (Skeyword stack S16)
run' S16 stack (TKeyword "¬") = shift >> run S63 (Skeyword stack S16)
run' S17 stack (TKeyword "-") = shift >> run S28 (Skeyword stack S17)
run' S17 stack (TKeyword "⋆") = shift >> run S45 (Skeyword stack S17)
run' S17 stack (TKeyword "⟨") = shift >> run S31 (Skeyword stack S17)
run' S17 stack (TKeyword "λ") = shift >> run S46 (Skeyword stack S17)
run' S17 stack (TKeyword "(") = shift >> run S49 (Skeyword stack S17)
run' S17 stack (TKeyword "∃") = shift >> run S52 (Skeyword stack S17)
run' S17 stack (TKeyword "[") = shift >> run S54 (Skeyword stack S17)
run' S17 stack (TInteger arg) = shift >> run S57 (Sinteger stack S17 arg)
run' S17 stack (TString arg) = shift >> run S36 (Sstring stack S17 arg)
run' S17 stack (TKeyword "⟦") = shift >> run S37 (Skeyword stack S17)
run' S17 stack (TKeyword "~") = shift >> run S59 (Skeyword stack S17)
run' S17 stack (TId arg) = shift >> run S39 (Sid stack S17 arg)
run' S17 stack (TKeyword "{") = shift >> run S42 (Skeyword stack S17)
run' S17 stack (TReal arg) = shift >> run S44 (Sreal stack S17 arg)
run' S17 stack (TKeyword "⟮") = shift >> run S43 (Skeyword stack S17)
run' S17 stack (TKeyword "¬") = shift >> run S63 (Skeyword stack S17)
run' S18 stack (TKeyword "⟨") = shift >> run S13 (Skeyword stack S18)
run' S18 stack (TKeyword "=") = case stack of
    (Slambdabinder stack s x0) ->
        gotoLambdabinders s (Slambdabinders stack s (reduction116 x0))
run' S18 stack (TKeyword ".") = case stack of
    (Slambdabinder stack s x0) ->
        gotoLambdabinders s (Slambdabinders stack s (reduction116 x0))
run' S18 stack (TId arg) = gotoStructbind S18 (Sstructbind stack S18 reduction91)
run' S19 stack (TId arg) = shift >> run S10 (Sid stack S19 arg)
run' S20 stack (TId arg) = shift >> run S67 (Sid stack S20 arg)
run' S21 stack (TKeyword "import") = case stack of
    (Snames (Skeyword (Sid (Skeyword stack s) _ x0) _) _ x1) ->
        gotoStatement s (Sstatement stack s (reduction4 x0 x1))
run' S21 stack (TKeyword "def") = case stack of
    (Snames (Skeyword (Sid (Skeyword stack s) _ x0) _) _ x1) ->
        gotoStatement s (Sstatement stack s (reduction4 x0 x1))
run' S21 stack (TKeyword "export") = case stack of
    (Snames (Skeyword (Sid (Skeyword stack s) _ x0) _) _ x1) ->
        gotoStatement s (Sstatement stack s (reduction4 x0 x1))
run' S21 stack TEnd = case stack of
    (Snames (Skeyword (Sid (Skeyword stack s) _ x0) _) _ x1) ->
        gotoStatement s (Sstatement stack s (reduction4 x0 x1))
run' S22 stack (TKeyword "⟩") = shift >> run S68 (Skeyword stack S22)
run' S23 stack (TId arg) = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoStructbind s (Sstructbind stack s (reduction92 ))
run' S24 stack (TKeyword "⟩") = case stack of
    (Sid stack s x0) ->
        gotoLabbind s (Slabbind stack s (reduction102 x0))
run' S24 stack (TKeyword ",") = case stack of
    (Sid stack s x0) ->
        gotoLabbind s (Slabbind stack s (reduction102 x0))
run' S25 stack (TKeyword "⟩") = case stack of
    (Slabbind stack s x0) ->
        gotoLabbinds s (Slabbinds stack s (reduction100 x0))
run' S25 stack (TKeyword ",") = shift >> run S69 (Skeyword stack S25)
run' S26 stack (TKeyword "=") = case stack of
    (Sid (Sstructbind stack s x0) _ x1) ->
        gotoLambdabinder s (Slambdabinder stack s (reduction118 x0 x1))
run' S26 stack (TKeyword ".") = case stack of
    (Sid (Sstructbind stack s x0) _ x1) ->
        gotoLambdabinder s (Slambdabinder stack s (reduction118 x0 x1))
run' S26 stack (TKeyword "⟨") = case stack of
    (Sid (Sstructbind stack s x0) _ x1) ->
        gotoLambdabinder s (Slambdabinder stack s (reduction118 x0 x1))
run' S26 stack (TId arg) = case stack of
    (Sid (Sstructbind stack s x0) _ x1) ->
        gotoLambdabinder s (Slambdabinder stack s (reduction118 x0 x1))
run' S27 stack (TKeyword "-") = shift >> run S28 (Skeyword stack S27)
run' S27 stack (TKeyword "⋆") = shift >> run S45 (Skeyword stack S27)
run' S27 stack (TKeyword "⟨") = shift >> run S31 (Skeyword stack S27)
run' S27 stack (TKeyword "λ") = shift >> run S46 (Skeyword stack S27)
run' S27 stack (TKeyword "(") = shift >> run S49 (Skeyword stack S27)
run' S27 stack (TKeyword "∃") = shift >> run S52 (Skeyword stack S27)
run' S27 stack (TKeyword "[") = shift >> run S54 (Skeyword stack S27)
run' S27 stack (TInteger arg) = shift >> run S57 (Sinteger stack S27 arg)
run' S27 stack (TString arg) = shift >> run S36 (Sstring stack S27 arg)
run' S27 stack (TKeyword "⟦") = shift >> run S37 (Skeyword stack S27)
run' S27 stack (TKeyword "~") = shift >> run S59 (Skeyword stack S27)
run' S27 stack (TId arg) = shift >> run S39 (Sid stack S27 arg)
run' S27 stack (TKeyword "{") = shift >> run S42 (Skeyword stack S27)
run' S27 stack (TReal arg) = shift >> run S44 (Sreal stack S27 arg)
run' S27 stack (TKeyword "⟮") = shift >> run S43 (Skeyword stack S27)
run' S27 stack (TKeyword "¬") = shift >> run S63 (Skeyword stack S27)
run' S28 stack (TKeyword "-") = shift >> run S28 (Skeyword stack S28)
run' S28 stack (TKeyword "⋆") = shift >> run S45 (Skeyword stack S28)
run' S28 stack (TKeyword "⟨") = shift >> run S31 (Skeyword stack S28)
run' S28 stack (TKeyword "λ") = shift >> run S46 (Skeyword stack S28)
run' S28 stack (TKeyword "(") = shift >> run S49 (Skeyword stack S28)
run' S28 stack (TKeyword "∃") = shift >> run S52 (Skeyword stack S28)
run' S28 stack (TKeyword "[") = shift >> run S54 (Skeyword stack S28)
run' S28 stack (TInteger arg) = shift >> run S57 (Sinteger stack S28 arg)
run' S28 stack (TString arg) = shift >> run S36 (Sstring stack S28 arg)
run' S28 stack (TKeyword "⟦") = shift >> run S37 (Skeyword stack S28)
run' S28 stack (TKeyword "~") = shift >> run S59 (Skeyword stack S28)
run' S28 stack (TId arg) = shift >> run S39 (Sid stack S28 arg)
run' S28 stack (TKeyword "{") = shift >> run S42 (Skeyword stack S28)
run' S28 stack (TReal arg) = shift >> run S44 (Sreal stack S28 arg)
run' S28 stack (TKeyword "⟮") = shift >> run S43 (Skeyword stack S28)
run' S28 stack (TKeyword "¬") = shift >> run S63 (Skeyword stack S28)
run' S29 stack (TKeyword "-") = case stack of
    (Sprim stack s x0) ->
        gotoCall s (Scall stack s (reduction68 x0))
run' S29 stack (TKeyword "[") = case stack of
    (Sprim stack s x0) ->
        gotoCall s (Scall stack s (reduction68 x0))
run' S29 stack (TKeyword "^") = case stack of
    (Sprim stack s x0) ->
        gotoCall s (Scall stack s (reduction68 x0))
run' S29 stack (TKeyword ")") = case stack of
    (Sprim stack s x0) ->
        gotoCall s (Scall stack s (reduction68 x0))
run' S29 stack (TKeyword "⟦") = case stack of
    (Sprim stack s x0) ->
        gotoCall s (Scall stack s (reduction68 x0))
run' S29 stack (TKeyword "≤") = case stack of
    (Sprim stack s x0) ->
        gotoCall s (Scall stack s (reduction68 x0))
run' S29 stack (TKeyword "<") = case stack of
    (Sprim stack s x0) ->
        gotoCall s (Scall stack s (reduction68 x0))
run' S29 stack (TKeyword "≠") = case stack of
    (Sprim stack s x0) ->
        gotoCall s (Scall stack s (reduction68 x0))
run' S29 stack (TKeyword "{") = case stack of
    (Sprim stack s x0) ->
        gotoCall s (Scall stack s (reduction68 x0))
run' S29 stack (TKeyword "⟮") = case stack of
    (Sprim stack s x0) ->
        gotoCall s (Scall stack s (reduction68 x0))
run' S29 stack (TReal arg) = case stack of
    (Sprim stack s x0) ->
        gotoCall s (Scall stack s (reduction68 x0))
run' S29 stack (TKeyword "⋆") = case stack of
    (Sprim stack s x0) ->
        gotoCall s (Scall stack s (reduction68 x0))
run' S29 stack (TKeyword "*") = case stack of
    (Sprim stack s x0) ->
        gotoCall s (Scall stack s (reduction68 x0))
run' S29 stack (TKeyword "def") = case stack of
    (Sprim stack s x0) ->
        gotoCall s (Scall stack s (reduction68 x0))
run' S29 stack TEnd = case stack of
    (Sprim stack s x0) ->
        gotoCall s (Scall stack s (reduction68 x0))
run' S29 stack (TInteger arg) = case stack of
    (Sprim stack s x0) ->
        gotoCall s (Scall stack s (reduction68 x0))
run' S29 stack (TKeyword "∘") = case stack of
    (Sprim stack s x0) ->
        gotoCall s (Scall stack s (reduction68 x0))
run' S29 stack (TKeyword "⟧") = case stack of
    (Sprim stack s x0) ->
        gotoCall s (Scall stack s (reduction68 x0))
run' S29 stack (TKeyword "≥") = case stack of
    (Sprim stack s x0) ->
        gotoCall s (Scall stack s (reduction68 x0))
run' S29 stack (TKeyword "|") = case stack of
    (Sprim stack s x0) ->
        gotoCall s (Scall stack s (reduction68 x0))
run' S29 stack (TKeyword "=") = case stack of
    (Sprim stack s x0) ->
        gotoCall s (Scall stack s (reduction68 x0))
run' S29 stack (TKeyword "≡") = case stack of
    (Sprim stack s x0) ->
        gotoCall s (Scall stack s (reduction68 x0))
run' S29 stack (TKeyword "⟯") = case stack of
    (Sprim stack s x0) ->
        gotoCall s (Scall stack s (reduction68 x0))
run' S29 stack (TKeyword "→") = case stack of
    (Sprim stack s x0) ->
        gotoCall s (Scall stack s (reduction68 x0))
run' S29 stack (TKeyword "+") = case stack of
    (Sprim stack s x0) ->
        gotoCall s (Scall stack s (reduction68 x0))
run' S29 stack (TKeyword "⟨") = case stack of
    (Sprim stack s x0) ->
        gotoCall s (Scall stack s (reduction68 x0))
run' S29 stack (TKeyword "export") = case stack of
    (Sprim stack s x0) ->
        gotoCall s (Scall stack s (reduction68 x0))
run' S29 stack (TId arg) = case stack of
    (Sprim stack s x0) ->
        gotoCall s (Scall stack s (reduction68 x0))
run' S29 stack (TString arg) = case stack of
    (Sprim stack s x0) ->
        gotoCall s (Scall stack s (reduction68 x0))
run' S29 stack (TKeyword "∧") = case stack of
    (Sprim stack s x0) ->
        gotoCall s (Scall stack s (reduction68 x0))
run' S29 stack (TKeyword ">") = case stack of
    (Sprim stack s x0) ->
        gotoCall s (Scall stack s (reduction68 x0))
run' S29 stack (TKeyword "}") = case stack of
    (Sprim stack s x0) ->
        gotoCall s (Scall stack s (reduction68 x0))
run' S29 stack (TKeyword ":") = case stack of
    (Sprim stack s x0) ->
        gotoCall s (Scall stack s (reduction68 x0))
run' S29 stack (TKeyword "/") = case stack of
    (Sprim stack s x0) ->
        gotoCall s (Scall stack s (reduction68 x0))
run' S29 stack (TKeyword "⊓") = case stack of
    (Sprim stack s x0) ->
        gotoCall s (Scall stack s (reduction68 x0))
run' S29 stack (TKeyword "(") = case stack of
    (Sprim stack s x0) ->
        gotoCall s (Scall stack s (reduction68 x0))
run' S29 stack (TKeyword "import") = case stack of
    (Sprim stack s x0) ->
        gotoCall s (Scall stack s (reduction68 x0))
run' S29 stack (TKeyword "∨") = case stack of
    (Sprim stack s x0) ->
        gotoCall s (Scall stack s (reduction68 x0))
run' S29 stack (TKeyword "⟩") = case stack of
    (Sprim stack s x0) ->
        gotoCall s (Scall stack s (reduction68 x0))
run' S29 stack (TKeyword "]") = case stack of
    (Sprim stack s x0) ->
        gotoCall s (Scall stack s (reduction68 x0))
run' S29 stack (TKeyword ";") = case stack of
    (Sprim stack s x0) ->
        gotoCall s (Scall stack s (reduction68 x0))
run' S29 stack (TKeyword "⊔") = case stack of
    (Sprim stack s x0) ->
        gotoCall s (Scall stack s (reduction68 x0))
run' S29 stack (TKeyword ",") = case stack of
    (Sprim stack s x0) ->
        gotoCall s (Scall stack s (reduction68 x0))
run' S30 stack (TKeyword "export") = case stack of
    (Smonad stack s x0) ->
        gotoLambda s (Slambda stack s (reduction28 x0))
run' S30 stack (TKeyword "⟩") = case stack of
    (Smonad stack s x0) ->
        gotoLambda s (Slambda stack s (reduction28 x0))
run' S30 stack (TKeyword "def") = case stack of
    (Smonad stack s x0) ->
        gotoLambda s (Slambda stack s (reduction28 x0))
run' S30 stack (TKeyword ")") = case stack of
    (Smonad stack s x0) ->
        gotoLambda s (Slambda stack s (reduction28 x0))
run' S30 stack (TKeyword "]") = case stack of
    (Smonad stack s x0) ->
        gotoLambda s (Slambda stack s (reduction28 x0))
run' S30 stack TEnd = case stack of
    (Smonad stack s x0) ->
        gotoLambda s (Slambda stack s (reduction28 x0))
run' S30 stack (TKeyword "import") = case stack of
    (Smonad stack s x0) ->
        gotoLambda s (Slambda stack s (reduction28 x0))
run' S30 stack (TKeyword "⟧") = case stack of
    (Smonad stack s x0) ->
        gotoLambda s (Slambda stack s (reduction28 x0))
run' S30 stack (TKeyword "|") = case stack of
    (Smonad stack s x0) ->
        gotoLambda s (Slambda stack s (reduction28 x0))
run' S30 stack (TKeyword "}") = case stack of
    (Smonad stack s x0) ->
        gotoLambda s (Slambda stack s (reduction28 x0))
run' S30 stack (TKeyword "≡") = case stack of
    (Smonad stack s x0) ->
        gotoLambda s (Slambda stack s (reduction28 x0))
run' S30 stack (TKeyword ":") = case stack of
    (Smonad stack s x0) ->
        gotoLambda s (Slambda stack s (reduction28 x0))
run' S30 stack (TKeyword "⟯") = case stack of
    (Smonad stack s x0) ->
        gotoLambda s (Slambda stack s (reduction28 x0))
run' S30 stack (TKeyword "→") = case stack of
    (Smonad stack s x0) ->
        gotoLambda s (Slambda stack s (reduction28 x0))
run' S30 stack (TKeyword ",") = case stack of
    (Smonad stack s x0) ->
        gotoLambda s (Slambda stack s (reduction28 x0))
run' S31 stack (TKeyword "⟩") = shift >> run S74 (Skeyword stack S31)
run' S31 stack (TId arg) = shift >> run S75 (Sid stack S31 arg)
run' S32 stack (TKeyword "import") = case stack of
    (Sdisj2 stack s x0) ->
        gotoConj2 s (Sconj2 stack s (reduction41 x0))
run' S32 stack (TKeyword "⟧") = case stack of
    (Sdisj2 stack s x0) ->
        gotoConj2 s (Sconj2 stack s (reduction41 x0))
run' S32 stack (TKeyword ";") = case stack of
    (Sdisj2 stack s x0) ->
        gotoConj2 s (Sconj2 stack s (reduction41 x0))
run' S32 stack (TKeyword "⟩") = case stack of
    (Sdisj2 stack s x0) ->
        gotoConj2 s (Sconj2 stack s (reduction41 x0))
run' S32 stack (TKeyword "def") = case stack of
    (Sdisj2 stack s x0) ->
        gotoConj2 s (Sconj2 stack s (reduction41 x0))
run' S32 stack (TKeyword "export") = case stack of
    (Sdisj2 stack s x0) ->
        gotoConj2 s (Sconj2 stack s (reduction41 x0))
run' S32 stack (TKeyword ")") = case stack of
    (Sdisj2 stack s x0) ->
        gotoConj2 s (Sconj2 stack s (reduction41 x0))
run' S32 stack (TKeyword "|") = case stack of
    (Sdisj2 stack s x0) ->
        gotoConj2 s (Sconj2 stack s (reduction41 x0))
run' S32 stack (TKeyword "]") = case stack of
    (Sdisj2 stack s x0) ->
        gotoConj2 s (Sconj2 stack s (reduction41 x0))
run' S32 stack (TKeyword "}") = case stack of
    (Sdisj2 stack s x0) ->
        gotoConj2 s (Sconj2 stack s (reduction41 x0))
run' S32 stack (TKeyword "≡") = case stack of
    (Sdisj2 stack s x0) ->
        gotoConj2 s (Sconj2 stack s (reduction41 x0))
run' S32 stack (TKeyword ":") = case stack of
    (Sdisj2 stack s x0) ->
        gotoConj2 s (Sconj2 stack s (reduction41 x0))
run' S32 stack TEnd = case stack of
    (Sdisj2 stack s x0) ->
        gotoConj2 s (Sconj2 stack s (reduction41 x0))
run' S32 stack (TKeyword "⊔") = case stack of
    (Sdisj2 stack s x0) ->
        gotoConj2 s (Sconj2 stack s (reduction41 x0))
run' S32 stack (TKeyword "⟯") = case stack of
    (Sdisj2 stack s x0) ->
        gotoConj2 s (Sconj2 stack s (reduction41 x0))
run' S32 stack (TKeyword "→") = case stack of
    (Sdisj2 stack s x0) ->
        gotoConj2 s (Sconj2 stack s (reduction41 x0))
run' S32 stack (TKeyword "⊓") = shift >> run S76 (Skeyword stack S32)
run' S32 stack (TKeyword ",") = case stack of
    (Sdisj2 stack s x0) ->
        gotoConj2 s (Sconj2 stack s (reduction41 x0))
run' S33 stack (TKeyword "-") = case stack of
    (Sprefix stack s x0) ->
        gotoMul s (Smul stack s (reduction61 x0))
run' S33 stack (TKeyword "+") = case stack of
    (Sprefix stack s x0) ->
        gotoMul s (Smul stack s (reduction61 x0))
run' S33 stack (TKeyword ")") = case stack of
    (Sprefix stack s x0) ->
        gotoMul s (Smul stack s (reduction61 x0))
run' S33 stack (TKeyword "export") = case stack of
    (Sprefix stack s x0) ->
        gotoMul s (Smul stack s (reduction61 x0))
run' S33 stack (TKeyword "import") = case stack of
    (Sprefix stack s x0) ->
        gotoMul s (Smul stack s (reduction61 x0))
run' S33 stack (TKeyword "∧") = case stack of
    (Sprefix stack s x0) ->
        gotoMul s (Smul stack s (reduction61 x0))
run' S33 stack (TKeyword "≤") = case stack of
    (Sprefix stack s x0) ->
        gotoMul s (Smul stack s (reduction61 x0))
run' S33 stack (TKeyword ">") = case stack of
    (Sprefix stack s x0) ->
        gotoMul s (Smul stack s (reduction61 x0))
run' S33 stack (TKeyword "<") = case stack of
    (Sprefix stack s x0) ->
        gotoMul s (Smul stack s (reduction61 x0))
run' S33 stack (TKeyword "}") = case stack of
    (Sprefix stack s x0) ->
        gotoMul s (Smul stack s (reduction61 x0))
run' S33 stack (TKeyword "≠") = case stack of
    (Sprefix stack s x0) ->
        gotoMul s (Smul stack s (reduction61 x0))
run' S33 stack (TKeyword ":") = case stack of
    (Sprefix stack s x0) ->
        gotoMul s (Smul stack s (reduction61 x0))
run' S33 stack (TKeyword "/") = shift >> run S77 (Skeyword stack S33)
run' S33 stack (TKeyword "⊓") = case stack of
    (Sprefix stack s x0) ->
        gotoMul s (Smul stack s (reduction61 x0))
run' S33 stack (TKeyword "*") = shift >> run S78 (Skeyword stack S33)
run' S33 stack (TKeyword "def") = case stack of
    (Sprefix stack s x0) ->
        gotoMul s (Smul stack s (reduction61 x0))
run' S33 stack (TKeyword "∨") = case stack of
    (Sprefix stack s x0) ->
        gotoMul s (Smul stack s (reduction61 x0))
run' S33 stack (TKeyword "⟩") = case stack of
    (Sprefix stack s x0) ->
        gotoMul s (Smul stack s (reduction61 x0))
run' S33 stack (TKeyword "]") = case stack of
    (Sprefix stack s x0) ->
        gotoMul s (Smul stack s (reduction61 x0))
run' S33 stack TEnd = case stack of
    (Sprefix stack s x0) ->
        gotoMul s (Smul stack s (reduction61 x0))
run' S33 stack (TKeyword "∘") = case stack of
    (Sprefix stack s x0) ->
        gotoMul s (Smul stack s (reduction61 x0))
run' S33 stack (TKeyword "⟧") = case stack of
    (Sprefix stack s x0) ->
        gotoMul s (Smul stack s (reduction61 x0))
run' S33 stack (TKeyword "≥") = case stack of
    (Sprefix stack s x0) ->
        gotoMul s (Smul stack s (reduction61 x0))
run' S33 stack (TKeyword "|") = case stack of
    (Sprefix stack s x0) ->
        gotoMul s (Smul stack s (reduction61 x0))
run' S33 stack (TKeyword "=") = case stack of
    (Sprefix stack s x0) ->
        gotoMul s (Smul stack s (reduction61 x0))
run' S33 stack (TKeyword "≡") = case stack of
    (Sprefix stack s x0) ->
        gotoMul s (Smul stack s (reduction61 x0))
run' S33 stack (TKeyword ";") = case stack of
    (Sprefix stack s x0) ->
        gotoMul s (Smul stack s (reduction61 x0))
run' S33 stack (TKeyword "⊔") = case stack of
    (Sprefix stack s x0) ->
        gotoMul s (Smul stack s (reduction61 x0))
run' S33 stack (TKeyword "⟯") = case stack of
    (Sprefix stack s x0) ->
        gotoMul s (Smul stack s (reduction61 x0))
run' S33 stack (TKeyword "→") = case stack of
    (Sprefix stack s x0) ->
        gotoMul s (Smul stack s (reduction61 x0))
run' S33 stack (TKeyword ",") = case stack of
    (Sprefix stack s x0) ->
        gotoMul s (Smul stack s (reduction61 x0))
run' S34 stack (TKeyword "import") = case stack of
    (Sconj stack s x0) ->
        gotoDisj2 s (Sdisj2 stack s (reduction43 x0))
run' S34 stack (TKeyword "⟧") = case stack of
    (Sconj stack s x0) ->
        gotoDisj2 s (Sdisj2 stack s (reduction43 x0))
run' S34 stack (TKeyword "∨") = shift >> run S79 (Skeyword stack S34)
run' S34 stack (TKeyword ";") = case stack of
    (Sconj stack s x0) ->
        gotoDisj2 s (Sdisj2 stack s (reduction43 x0))
run' S34 stack (TKeyword "⟩") = case stack of
    (Sconj stack s x0) ->
        gotoDisj2 s (Sdisj2 stack s (reduction43 x0))
run' S34 stack (TKeyword "def") = case stack of
    (Sconj stack s x0) ->
        gotoDisj2 s (Sdisj2 stack s (reduction43 x0))
run' S34 stack (TKeyword "export") = case stack of
    (Sconj stack s x0) ->
        gotoDisj2 s (Sdisj2 stack s (reduction43 x0))
run' S34 stack (TKeyword ")") = case stack of
    (Sconj stack s x0) ->
        gotoDisj2 s (Sdisj2 stack s (reduction43 x0))
run' S34 stack (TKeyword "|") = case stack of
    (Sconj stack s x0) ->
        gotoDisj2 s (Sdisj2 stack s (reduction43 x0))
run' S34 stack (TKeyword "]") = case stack of
    (Sconj stack s x0) ->
        gotoDisj2 s (Sdisj2 stack s (reduction43 x0))
run' S34 stack (TKeyword "}") = case stack of
    (Sconj stack s x0) ->
        gotoDisj2 s (Sdisj2 stack s (reduction43 x0))
run' S34 stack (TKeyword "≡") = case stack of
    (Sconj stack s x0) ->
        gotoDisj2 s (Sdisj2 stack s (reduction43 x0))
run' S34 stack (TKeyword ":") = case stack of
    (Sconj stack s x0) ->
        gotoDisj2 s (Sdisj2 stack s (reduction43 x0))
run' S34 stack TEnd = case stack of
    (Sconj stack s x0) ->
        gotoDisj2 s (Sdisj2 stack s (reduction43 x0))
run' S34 stack (TKeyword "⊔") = case stack of
    (Sconj stack s x0) ->
        gotoDisj2 s (Sdisj2 stack s (reduction43 x0))
run' S34 stack (TKeyword "⟯") = case stack of
    (Sconj stack s x0) ->
        gotoDisj2 s (Sdisj2 stack s (reduction43 x0))
run' S34 stack (TKeyword "→") = case stack of
    (Sconj stack s x0) ->
        gotoDisj2 s (Sdisj2 stack s (reduction43 x0))
run' S34 stack (TKeyword "⊓") = case stack of
    (Sconj stack s x0) ->
        gotoDisj2 s (Sdisj2 stack s (reduction43 x0))
run' S34 stack (TKeyword ",") = case stack of
    (Sconj stack s x0) ->
        gotoDisj2 s (Sdisj2 stack s (reduction43 x0))
run' S35 stack (TKeyword "import") = case stack of
    (Sdisj stack s x0) ->
        gotoCocase s (Scocase stack s (reduction36 x0))
run' S35 stack (TKeyword "⟧") = case stack of
    (Sdisj stack s x0) ->
        gotoCocase s (Scocase stack s (reduction36 x0))
run' S35 stack (TKeyword ";") = case stack of
    (Sdisj stack s x0) ->
        gotoCocase s (Scocase stack s (reduction36 x0))
run' S35 stack (TKeyword "⟩") = case stack of
    (Sdisj stack s x0) ->
        gotoCocase s (Scocase stack s (reduction36 x0))
run' S35 stack (TKeyword "def") = case stack of
    (Sdisj stack s x0) ->
        gotoCocase s (Scocase stack s (reduction36 x0))
run' S35 stack (TKeyword "export") = case stack of
    (Sdisj stack s x0) ->
        gotoCocase s (Scocase stack s (reduction36 x0))
run' S35 stack (TKeyword ")") = case stack of
    (Sdisj stack s x0) ->
        gotoCocase s (Scocase stack s (reduction36 x0))
run' S35 stack (TKeyword "|") = case stack of
    (Sdisj stack s x0) ->
        gotoCocase s (Scocase stack s (reduction36 x0))
run' S35 stack (TKeyword "]") = case stack of
    (Sdisj stack s x0) ->
        gotoCocase s (Scocase stack s (reduction36 x0))
run' S35 stack (TKeyword "}") = case stack of
    (Sdisj stack s x0) ->
        gotoCocase s (Scocase stack s (reduction36 x0))
run' S35 stack (TKeyword "≡") = case stack of
    (Sdisj stack s x0) ->
        gotoCocase s (Scocase stack s (reduction36 x0))
run' S35 stack (TKeyword ":") = case stack of
    (Sdisj stack s x0) ->
        gotoCocase s (Scocase stack s (reduction36 x0))
run' S35 stack TEnd = case stack of
    (Sdisj stack s x0) ->
        gotoCocase s (Scocase stack s (reduction36 x0))
run' S35 stack (TKeyword "⟯") = case stack of
    (Sdisj stack s x0) ->
        gotoCocase s (Scocase stack s (reduction36 x0))
run' S35 stack (TKeyword "→") = case stack of
    (Sdisj stack s x0) ->
        gotoCocase s (Scocase stack s (reduction36 x0))
run' S35 stack (TKeyword ",") = case stack of
    (Sdisj stack s x0) ->
        gotoCocase s (Scocase stack s (reduction36 x0))
run' S36 stack (TKeyword "-") = case stack of
    (Sstring stack s x0) ->
        gotoPrim s (Sprim stack s (reduction77 x0))
run' S36 stack (TKeyword "[") = case stack of
    (Sstring stack s x0) ->
        gotoPrim s (Sprim stack s (reduction77 x0))
run' S36 stack (TKeyword "^") = case stack of
    (Sstring stack s x0) ->
        gotoPrim s (Sprim stack s (reduction77 x0))
run' S36 stack (TKeyword ")") = case stack of
    (Sstring stack s x0) ->
        gotoPrim s (Sprim stack s (reduction77 x0))
run' S36 stack (TKeyword "⟦") = case stack of
    (Sstring stack s x0) ->
        gotoPrim s (Sprim stack s (reduction77 x0))
run' S36 stack (TKeyword "≤") = case stack of
    (Sstring stack s x0) ->
        gotoPrim s (Sprim stack s (reduction77 x0))
run' S36 stack (TKeyword "<") = case stack of
    (Sstring stack s x0) ->
        gotoPrim s (Sprim stack s (reduction77 x0))
run' S36 stack (TKeyword "≠") = case stack of
    (Sstring stack s x0) ->
        gotoPrim s (Sprim stack s (reduction77 x0))
run' S36 stack (TKeyword "{") = case stack of
    (Sstring stack s x0) ->
        gotoPrim s (Sprim stack s (reduction77 x0))
run' S36 stack (TKeyword "⟮") = case stack of
    (Sstring stack s x0) ->
        gotoPrim s (Sprim stack s (reduction77 x0))
run' S36 stack (TReal arg) = case stack of
    (Sstring stack s x0) ->
        gotoPrim s (Sprim stack s (reduction77 x0))
run' S36 stack (TKeyword "⋆") = case stack of
    (Sstring stack s x0) ->
        gotoPrim s (Sprim stack s (reduction77 x0))
run' S36 stack (TKeyword "*") = case stack of
    (Sstring stack s x0) ->
        gotoPrim s (Sprim stack s (reduction77 x0))
run' S36 stack (TKeyword "def") = case stack of
    (Sstring stack s x0) ->
        gotoPrim s (Sprim stack s (reduction77 x0))
run' S36 stack TEnd = case stack of
    (Sstring stack s x0) ->
        gotoPrim s (Sprim stack s (reduction77 x0))
run' S36 stack (TInteger arg) = case stack of
    (Sstring stack s x0) ->
        gotoPrim s (Sprim stack s (reduction77 x0))
run' S36 stack (TKeyword "∘") = case stack of
    (Sstring stack s x0) ->
        gotoPrim s (Sprim stack s (reduction77 x0))
run' S36 stack (TKeyword "⟧") = case stack of
    (Sstring stack s x0) ->
        gotoPrim s (Sprim stack s (reduction77 x0))
run' S36 stack (TKeyword "≥") = case stack of
    (Sstring stack s x0) ->
        gotoPrim s (Sprim stack s (reduction77 x0))
run' S36 stack (TKeyword "|") = case stack of
    (Sstring stack s x0) ->
        gotoPrim s (Sprim stack s (reduction77 x0))
run' S36 stack (TKeyword "=") = case stack of
    (Sstring stack s x0) ->
        gotoPrim s (Sprim stack s (reduction77 x0))
run' S36 stack (TKeyword "≡") = case stack of
    (Sstring stack s x0) ->
        gotoPrim s (Sprim stack s (reduction77 x0))
run' S36 stack (TKeyword "⟯") = case stack of
    (Sstring stack s x0) ->
        gotoPrim s (Sprim stack s (reduction77 x0))
run' S36 stack (TKeyword "→") = case stack of
    (Sstring stack s x0) ->
        gotoPrim s (Sprim stack s (reduction77 x0))
run' S36 stack (TKeyword "+") = case stack of
    (Sstring stack s x0) ->
        gotoPrim s (Sprim stack s (reduction77 x0))
run' S36 stack (TKeyword "⟨") = case stack of
    (Sstring stack s x0) ->
        gotoPrim s (Sprim stack s (reduction77 x0))
run' S36 stack (TKeyword "export") = case stack of
    (Sstring stack s x0) ->
        gotoPrim s (Sprim stack s (reduction77 x0))
run' S36 stack (TId arg) = case stack of
    (Sstring stack s x0) ->
        gotoPrim s (Sprim stack s (reduction77 x0))
run' S36 stack (TString arg) = case stack of
    (Sstring stack s x0) ->
        gotoPrim s (Sprim stack s (reduction77 x0))
run' S36 stack (TKeyword "∧") = case stack of
    (Sstring stack s x0) ->
        gotoPrim s (Sprim stack s (reduction77 x0))
run' S36 stack (TKeyword ">") = case stack of
    (Sstring stack s x0) ->
        gotoPrim s (Sprim stack s (reduction77 x0))
run' S36 stack (TKeyword "}") = case stack of
    (Sstring stack s x0) ->
        gotoPrim s (Sprim stack s (reduction77 x0))
run' S36 stack (TKeyword ":") = case stack of
    (Sstring stack s x0) ->
        gotoPrim s (Sprim stack s (reduction77 x0))
run' S36 stack (TKeyword "/") = case stack of
    (Sstring stack s x0) ->
        gotoPrim s (Sprim stack s (reduction77 x0))
run' S36 stack (TKeyword "⊓") = case stack of
    (Sstring stack s x0) ->
        gotoPrim s (Sprim stack s (reduction77 x0))
run' S36 stack (TKeyword "(") = case stack of
    (Sstring stack s x0) ->
        gotoPrim s (Sprim stack s (reduction77 x0))
run' S36 stack (TKeyword "import") = case stack of
    (Sstring stack s x0) ->
        gotoPrim s (Sprim stack s (reduction77 x0))
run' S36 stack (TKeyword "∨") = case stack of
    (Sstring stack s x0) ->
        gotoPrim s (Sprim stack s (reduction77 x0))
run' S36 stack (TKeyword "⟩") = case stack of
    (Sstring stack s x0) ->
        gotoPrim s (Sprim stack s (reduction77 x0))
run' S36 stack (TKeyword "]") = case stack of
    (Sstring stack s x0) ->
        gotoPrim s (Sprim stack s (reduction77 x0))
run' S36 stack (TKeyword ";") = case stack of
    (Sstring stack s x0) ->
        gotoPrim s (Sprim stack s (reduction77 x0))
run' S36 stack (TKeyword "⊔") = case stack of
    (Sstring stack s x0) ->
        gotoPrim s (Sprim stack s (reduction77 x0))
run' S36 stack (TKeyword ",") = case stack of
    (Sstring stack s x0) ->
        gotoPrim s (Sprim stack s (reduction77 x0))
run' S37 stack (TKeyword "-") = shift >> run S28 (Skeyword stack S37)
run' S37 stack (TKeyword "⋆") = shift >> run S45 (Skeyword stack S37)
run' S37 stack (TKeyword "⟨") = shift >> run S31 (Skeyword stack S37)
run' S37 stack (TKeyword "λ") = shift >> run S46 (Skeyword stack S37)
run' S37 stack (TKeyword "(") = shift >> run S49 (Skeyword stack S37)
run' S37 stack (TKeyword "∃") = shift >> run S52 (Skeyword stack S37)
run' S37 stack (TKeyword "[") = shift >> run S54 (Skeyword stack S37)
run' S37 stack (TInteger arg) = shift >> run S57 (Sinteger stack S37 arg)
run' S37 stack (TString arg) = shift >> run S36 (Sstring stack S37 arg)
run' S37 stack (TKeyword "⟦") = shift >> run S37 (Skeyword stack S37)
run' S37 stack (TKeyword "~") = shift >> run S59 (Skeyword stack S37)
run' S37 stack (TId arg) = shift >> run S39 (Sid stack S37 arg)
run' S37 stack (TKeyword "{") = shift >> run S42 (Skeyword stack S37)
run' S37 stack (TReal arg) = shift >> run S44 (Sreal stack S37 arg)
run' S37 stack (TKeyword "⟮") = shift >> run S43 (Skeyword stack S37)
run' S37 stack (TKeyword "¬") = shift >> run S63 (Skeyword stack S37)
run' S38 stack (TId arg) = shift >> run S39 (Sid stack S38 arg)
run' S38 stack (TString arg) = shift >> run S36 (Sstring stack S38 arg)
run' S38 stack (TKeyword "{") = shift >> run S42 (Skeyword stack S38)
run' S38 stack (TKeyword "⟮") = shift >> run S43 (Skeyword stack S38)
run' S38 stack (TReal arg) = shift >> run S44 (Sreal stack S38 arg)
run' S38 stack (TKeyword "⋆") = shift >> run S45 (Skeyword stack S38)
run' S38 stack (TKeyword "λ") = shift >> run S46 (Skeyword stack S38)
run' S38 stack (TKeyword "(") = shift >> run S81 (Skeyword stack S38)
run' S38 stack (TKeyword "∃") = shift >> run S52 (Skeyword stack S38)
run' S38 stack (TKeyword "[") = shift >> run S54 (Skeyword stack S38)
run' S38 stack (TInteger arg) = shift >> run S57 (Sinteger stack S38 arg)
run' S38 stack (TKeyword "~") = shift >> run S59 (Skeyword stack S38)
run' S38 stack (TKeyword "¬") = shift >> run S63 (Skeyword stack S38)
run' S39 stack (TKeyword "-") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S39 stack (TKeyword "[") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S39 stack (TKeyword "∨") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S39 stack (TKeyword "^") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S39 stack (TKeyword ")") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S39 stack (TKeyword "⟦") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S39 stack (TKeyword "≤") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S39 stack (TKeyword "<") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S39 stack (TKeyword "≠") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S39 stack (TKeyword "{") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S39 stack (TKeyword "⟮") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S39 stack (TReal arg) = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S39 stack (TKeyword "⋆") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S39 stack (TKeyword "*") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S39 stack (TKeyword "def") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S39 stack TEnd = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S39 stack (TInteger arg) = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S39 stack (TKeyword "∘") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S39 stack (TKeyword "⟧") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S39 stack (TKeyword "≥") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S39 stack (TKeyword "|") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S39 stack (TKeyword "=") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S39 stack (TKeyword "≡") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S39 stack (TKeyword "⟯") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S39 stack (TKeyword "→") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S39 stack (TKeyword "+") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S39 stack (TKeyword "⟨") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S39 stack (TKeyword "export") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S39 stack (TId arg) = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S39 stack (TString arg) = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S39 stack (TKeyword "∧") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S39 stack (TKeyword ">") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S39 stack (TKeyword "}") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S39 stack (TKeyword ":") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S39 stack (TKeyword "/") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S39 stack (TKeyword "⊓") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S39 stack (TKeyword "←") = shift >> run S84 (Skeyword stack S39)
run' S39 stack (TKeyword "import") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S39 stack (TKeyword "(") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S39 stack (TKeyword "⟩") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S39 stack (TKeyword "]") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S39 stack (TKeyword "↦") = shift >> run S83 (Skeyword stack S39)
run' S39 stack (TKeyword ";") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S39 stack (TKeyword "⊔") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S39 stack (TKeyword "is") = shift >> run S85 (Skeyword stack S39)
run' S39 stack (TKeyword ",") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S40 stack (TKeyword ")") = case stack of
    (Sadd stack s x0) ->
        gotoComp s (Scomp stack s (reduction56 x0))
run' S40 stack (TKeyword "export") = case stack of
    (Sadd stack s x0) ->
        gotoComp s (Scomp stack s (reduction56 x0))
run' S40 stack (TKeyword "import") = case stack of
    (Sadd stack s x0) ->
        gotoComp s (Scomp stack s (reduction56 x0))
run' S40 stack (TKeyword "∧") = case stack of
    (Sadd stack s x0) ->
        gotoComp s (Scomp stack s (reduction56 x0))
run' S40 stack (TKeyword "≤") = case stack of
    (Sadd stack s x0) ->
        gotoComp s (Scomp stack s (reduction56 x0))
run' S40 stack (TKeyword ">") = case stack of
    (Sadd stack s x0) ->
        gotoComp s (Scomp stack s (reduction56 x0))
run' S40 stack (TKeyword "<") = case stack of
    (Sadd stack s x0) ->
        gotoComp s (Scomp stack s (reduction56 x0))
run' S40 stack (TKeyword "}") = case stack of
    (Sadd stack s x0) ->
        gotoComp s (Scomp stack s (reduction56 x0))
run' S40 stack (TKeyword "≠") = case stack of
    (Sadd stack s x0) ->
        gotoComp s (Scomp stack s (reduction56 x0))
run' S40 stack (TKeyword ":") = case stack of
    (Sadd stack s x0) ->
        gotoComp s (Scomp stack s (reduction56 x0))
run' S40 stack (TKeyword "⊓") = case stack of
    (Sadd stack s x0) ->
        gotoComp s (Scomp stack s (reduction56 x0))
run' S40 stack (TKeyword "def") = case stack of
    (Sadd stack s x0) ->
        gotoComp s (Scomp stack s (reduction56 x0))
run' S40 stack (TKeyword "∨") = case stack of
    (Sadd stack s x0) ->
        gotoComp s (Scomp stack s (reduction56 x0))
run' S40 stack (TKeyword "⟩") = case stack of
    (Sadd stack s x0) ->
        gotoComp s (Scomp stack s (reduction56 x0))
run' S40 stack (TKeyword "]") = case stack of
    (Sadd stack s x0) ->
        gotoComp s (Scomp stack s (reduction56 x0))
run' S40 stack TEnd = case stack of
    (Sadd stack s x0) ->
        gotoComp s (Scomp stack s (reduction56 x0))
run' S40 stack (TKeyword "∘") = shift >> run S86 (Skeyword stack S40)
run' S40 stack (TKeyword "⟧") = case stack of
    (Sadd stack s x0) ->
        gotoComp s (Scomp stack s (reduction56 x0))
run' S40 stack (TKeyword "≥") = case stack of
    (Sadd stack s x0) ->
        gotoComp s (Scomp stack s (reduction56 x0))
run' S40 stack (TKeyword "|") = case stack of
    (Sadd stack s x0) ->
        gotoComp s (Scomp stack s (reduction56 x0))
run' S40 stack (TKeyword "=") = case stack of
    (Sadd stack s x0) ->
        gotoComp s (Scomp stack s (reduction56 x0))
run' S40 stack (TKeyword "≡") = case stack of
    (Sadd stack s x0) ->
        gotoComp s (Scomp stack s (reduction56 x0))
run' S40 stack (TKeyword ";") = case stack of
    (Sadd stack s x0) ->
        gotoComp s (Scomp stack s (reduction56 x0))
run' S40 stack (TKeyword "⊔") = case stack of
    (Sadd stack s x0) ->
        gotoComp s (Scomp stack s (reduction56 x0))
run' S40 stack (TKeyword "⟯") = case stack of
    (Sadd stack s x0) ->
        gotoComp s (Scomp stack s (reduction56 x0))
run' S40 stack (TKeyword "→") = case stack of
    (Sadd stack s x0) ->
        gotoComp s (Scomp stack s (reduction56 x0))
run' S40 stack (TKeyword ",") = case stack of
    (Sadd stack s x0) ->
        gotoComp s (Scomp stack s (reduction56 x0))
run' S41 stack (TKeyword "-") = case stack of
    (Scall stack s x0) ->
        gotoExp s (Sexp stack s (reduction66 x0))
run' S41 stack (TKeyword "[") = gotoStruct S41 (Sstruct stack S41 reduction85)
run' S41 stack (TKeyword "^") = shift >> run S89 (Skeyword stack S41)
run' S41 stack (TKeyword ")") = case stack of
    (Scall stack s x0) ->
        gotoExp s (Sexp stack s (reduction66 x0))
run' S41 stack (TKeyword "⟦") = shift >> run S87 (Skeyword stack S41)
run' S41 stack (TKeyword "≤") = case stack of
    (Scall stack s x0) ->
        gotoExp s (Sexp stack s (reduction66 x0))
run' S41 stack (TKeyword "<") = case stack of
    (Scall stack s x0) ->
        gotoExp s (Sexp stack s (reduction66 x0))
run' S41 stack (TKeyword "≠") = case stack of
    (Scall stack s x0) ->
        gotoExp s (Sexp stack s (reduction66 x0))
run' S41 stack (TKeyword "{") = gotoStruct S41 (Sstruct stack S41 reduction85)
run' S41 stack (TKeyword "⟮") = shift >> run S91 (Skeyword stack S41)
run' S41 stack (TReal arg) = gotoStruct S41 (Sstruct stack S41 reduction85)
run' S41 stack (TKeyword "⋆") = gotoStruct S41 (Sstruct stack S41 reduction85)
run' S41 stack (TKeyword "*") = case stack of
    (Scall stack s x0) ->
        gotoExp s (Sexp stack s (reduction66 x0))
run' S41 stack (TKeyword "def") = case stack of
    (Scall stack s x0) ->
        gotoExp s (Sexp stack s (reduction66 x0))
run' S41 stack TEnd = case stack of
    (Scall stack s x0) ->
        gotoExp s (Sexp stack s (reduction66 x0))
run' S41 stack (TInteger arg) = gotoStruct S41 (Sstruct stack S41 reduction85)
run' S41 stack (TKeyword "∘") = case stack of
    (Scall stack s x0) ->
        gotoExp s (Sexp stack s (reduction66 x0))
run' S41 stack (TKeyword "⟧") = case stack of
    (Scall stack s x0) ->
        gotoExp s (Sexp stack s (reduction66 x0))
run' S41 stack (TKeyword "≥") = case stack of
    (Scall stack s x0) ->
        gotoExp s (Sexp stack s (reduction66 x0))
run' S41 stack (TKeyword "|") = case stack of
    (Scall stack s x0) ->
        gotoExp s (Sexp stack s (reduction66 x0))
run' S41 stack (TKeyword "=") = case stack of
    (Scall stack s x0) ->
        gotoExp s (Sexp stack s (reduction66 x0))
run' S41 stack (TKeyword "≡") = case stack of
    (Scall stack s x0) ->
        gotoExp s (Sexp stack s (reduction66 x0))
run' S41 stack (TKeyword "⟯") = case stack of
    (Scall stack s x0) ->
        gotoExp s (Sexp stack s (reduction66 x0))
run' S41 stack (TKeyword "→") = case stack of
    (Scall stack s x0) ->
        gotoExp s (Sexp stack s (reduction66 x0))
run' S41 stack (TKeyword "+") = case stack of
    (Scall stack s x0) ->
        gotoExp s (Sexp stack s (reduction66 x0))
run' S41 stack (TKeyword "⟨") = shift >> run S90 (Skeyword stack S41)
run' S41 stack (TKeyword "export") = case stack of
    (Scall stack s x0) ->
        gotoExp s (Sexp stack s (reduction66 x0))
run' S41 stack (TId arg) = gotoStruct S41 (Sstruct stack S41 reduction85)
run' S41 stack (TString arg) = gotoStruct S41 (Sstruct stack S41 reduction85)
run' S41 stack (TKeyword "∧") = case stack of
    (Scall stack s x0) ->
        gotoExp s (Sexp stack s (reduction66 x0))
run' S41 stack (TKeyword ">") = case stack of
    (Scall stack s x0) ->
        gotoExp s (Sexp stack s (reduction66 x0))
run' S41 stack (TKeyword "}") = case stack of
    (Scall stack s x0) ->
        gotoExp s (Sexp stack s (reduction66 x0))
run' S41 stack (TKeyword ":") = case stack of
    (Scall stack s x0) ->
        gotoExp s (Sexp stack s (reduction66 x0))
run' S41 stack (TKeyword "/") = case stack of
    (Scall stack s x0) ->
        gotoExp s (Sexp stack s (reduction66 x0))
run' S41 stack (TKeyword "⊓") = case stack of
    (Scall stack s x0) ->
        gotoExp s (Sexp stack s (reduction66 x0))
run' S41 stack (TKeyword "(") = gotoStruct S41 (Sstruct stack S41 reduction85)
run' S41 stack (TKeyword "import") = case stack of
    (Scall stack s x0) ->
        gotoExp s (Sexp stack s (reduction66 x0))
run' S41 stack (TKeyword "∨") = case stack of
    (Scall stack s x0) ->
        gotoExp s (Sexp stack s (reduction66 x0))
run' S41 stack (TKeyword "⟩") = case stack of
    (Scall stack s x0) ->
        gotoExp s (Sexp stack s (reduction66 x0))
run' S41 stack (TKeyword "]") = case stack of
    (Scall stack s x0) ->
        gotoExp s (Sexp stack s (reduction66 x0))
run' S41 stack (TKeyword ";") = case stack of
    (Scall stack s x0) ->
        gotoExp s (Sexp stack s (reduction66 x0))
run' S41 stack (TKeyword "⊔") = case stack of
    (Scall stack s x0) ->
        gotoExp s (Sexp stack s (reduction66 x0))
run' S41 stack (TKeyword ",") = case stack of
    (Scall stack s x0) ->
        gotoExp s (Sexp stack s (reduction66 x0))
run' S42 stack (TReal arg) = shift >> run S44 (Sreal stack S42 arg)
run' S42 stack (TKeyword "⋆") = shift >> run S45 (Skeyword stack S42)
run' S42 stack (TKeyword "λ") = shift >> run S46 (Skeyword stack S42)
run' S42 stack (TKeyword "(") = shift >> run S96 (Skeyword stack S42)
run' S42 stack (TKeyword "∃") = shift >> run S52 (Skeyword stack S42)
run' S42 stack (TKeyword "[") = shift >> run S54 (Skeyword stack S42)
run' S42 stack (TInteger arg) = shift >> run S57 (Sinteger stack S42 arg)
run' S42 stack (TString arg) = shift >> run S36 (Sstring stack S42 arg)
run' S42 stack (TKeyword "~") = shift >> run S59 (Skeyword stack S42)
run' S42 stack (TId arg) = shift >> run S93 (Sid stack S42 arg)
run' S42 stack (TKeyword "}") = shift >> run S94 (Skeyword stack S42)
run' S42 stack (TKeyword "{") = shift >> run S42 (Skeyword stack S42)
run' S42 stack (TKeyword "⟮") = shift >> run S43 (Skeyword stack S42)
run' S42 stack (TKeyword "¬") = shift >> run S63 (Skeyword stack S42)
run' S43 stack (TId arg) = shift >> run S101 (Sid stack S43 arg)
run' S44 stack (TKeyword "-") = case stack of
    (Sreal stack s x0) ->
        gotoPrim s (Sprim stack s (reduction76 x0))
run' S44 stack (TKeyword "[") = case stack of
    (Sreal stack s x0) ->
        gotoPrim s (Sprim stack s (reduction76 x0))
run' S44 stack (TKeyword "^") = case stack of
    (Sreal stack s x0) ->
        gotoPrim s (Sprim stack s (reduction76 x0))
run' S44 stack (TKeyword ")") = case stack of
    (Sreal stack s x0) ->
        gotoPrim s (Sprim stack s (reduction76 x0))
run' S44 stack (TKeyword "⟦") = case stack of
    (Sreal stack s x0) ->
        gotoPrim s (Sprim stack s (reduction76 x0))
run' S44 stack (TKeyword "≤") = case stack of
    (Sreal stack s x0) ->
        gotoPrim s (Sprim stack s (reduction76 x0))
run' S44 stack (TKeyword "<") = case stack of
    (Sreal stack s x0) ->
        gotoPrim s (Sprim stack s (reduction76 x0))
run' S44 stack (TKeyword "≠") = case stack of
    (Sreal stack s x0) ->
        gotoPrim s (Sprim stack s (reduction76 x0))
run' S44 stack (TKeyword "{") = case stack of
    (Sreal stack s x0) ->
        gotoPrim s (Sprim stack s (reduction76 x0))
run' S44 stack (TKeyword "⟮") = case stack of
    (Sreal stack s x0) ->
        gotoPrim s (Sprim stack s (reduction76 x0))
run' S44 stack (TReal arg) = case stack of
    (Sreal stack s x0) ->
        gotoPrim s (Sprim stack s (reduction76 x0))
run' S44 stack (TKeyword "⋆") = case stack of
    (Sreal stack s x0) ->
        gotoPrim s (Sprim stack s (reduction76 x0))
run' S44 stack (TKeyword "*") = case stack of
    (Sreal stack s x0) ->
        gotoPrim s (Sprim stack s (reduction76 x0))
run' S44 stack (TKeyword "def") = case stack of
    (Sreal stack s x0) ->
        gotoPrim s (Sprim stack s (reduction76 x0))
run' S44 stack TEnd = case stack of
    (Sreal stack s x0) ->
        gotoPrim s (Sprim stack s (reduction76 x0))
run' S44 stack (TInteger arg) = case stack of
    (Sreal stack s x0) ->
        gotoPrim s (Sprim stack s (reduction76 x0))
run' S44 stack (TKeyword "∘") = case stack of
    (Sreal stack s x0) ->
        gotoPrim s (Sprim stack s (reduction76 x0))
run' S44 stack (TKeyword "⟧") = case stack of
    (Sreal stack s x0) ->
        gotoPrim s (Sprim stack s (reduction76 x0))
run' S44 stack (TKeyword "≥") = case stack of
    (Sreal stack s x0) ->
        gotoPrim s (Sprim stack s (reduction76 x0))
run' S44 stack (TKeyword "|") = case stack of
    (Sreal stack s x0) ->
        gotoPrim s (Sprim stack s (reduction76 x0))
run' S44 stack (TKeyword "=") = case stack of
    (Sreal stack s x0) ->
        gotoPrim s (Sprim stack s (reduction76 x0))
run' S44 stack (TKeyword "≡") = case stack of
    (Sreal stack s x0) ->
        gotoPrim s (Sprim stack s (reduction76 x0))
run' S44 stack (TKeyword "⟯") = case stack of
    (Sreal stack s x0) ->
        gotoPrim s (Sprim stack s (reduction76 x0))
run' S44 stack (TKeyword "→") = case stack of
    (Sreal stack s x0) ->
        gotoPrim s (Sprim stack s (reduction76 x0))
run' S44 stack (TKeyword "+") = case stack of
    (Sreal stack s x0) ->
        gotoPrim s (Sprim stack s (reduction76 x0))
run' S44 stack (TKeyword "⟨") = case stack of
    (Sreal stack s x0) ->
        gotoPrim s (Sprim stack s (reduction76 x0))
run' S44 stack (TKeyword "export") = case stack of
    (Sreal stack s x0) ->
        gotoPrim s (Sprim stack s (reduction76 x0))
run' S44 stack (TId arg) = case stack of
    (Sreal stack s x0) ->
        gotoPrim s (Sprim stack s (reduction76 x0))
run' S44 stack (TString arg) = case stack of
    (Sreal stack s x0) ->
        gotoPrim s (Sprim stack s (reduction76 x0))
run' S44 stack (TKeyword "∧") = case stack of
    (Sreal stack s x0) ->
        gotoPrim s (Sprim stack s (reduction76 x0))
run' S44 stack (TKeyword ">") = case stack of
    (Sreal stack s x0) ->
        gotoPrim s (Sprim stack s (reduction76 x0))
run' S44 stack (TKeyword "}") = case stack of
    (Sreal stack s x0) ->
        gotoPrim s (Sprim stack s (reduction76 x0))
run' S44 stack (TKeyword ":") = case stack of
    (Sreal stack s x0) ->
        gotoPrim s (Sprim stack s (reduction76 x0))
run' S44 stack (TKeyword "/") = case stack of
    (Sreal stack s x0) ->
        gotoPrim s (Sprim stack s (reduction76 x0))
run' S44 stack (TKeyword "⊓") = case stack of
    (Sreal stack s x0) ->
        gotoPrim s (Sprim stack s (reduction76 x0))
run' S44 stack (TKeyword "(") = case stack of
    (Sreal stack s x0) ->
        gotoPrim s (Sprim stack s (reduction76 x0))
run' S44 stack (TKeyword "import") = case stack of
    (Sreal stack s x0) ->
        gotoPrim s (Sprim stack s (reduction76 x0))
run' S44 stack (TKeyword "∨") = case stack of
    (Sreal stack s x0) ->
        gotoPrim s (Sprim stack s (reduction76 x0))
run' S44 stack (TKeyword "⟩") = case stack of
    (Sreal stack s x0) ->
        gotoPrim s (Sprim stack s (reduction76 x0))
run' S44 stack (TKeyword "]") = case stack of
    (Sreal stack s x0) ->
        gotoPrim s (Sprim stack s (reduction76 x0))
run' S44 stack (TKeyword ";") = case stack of
    (Sreal stack s x0) ->
        gotoPrim s (Sprim stack s (reduction76 x0))
run' S44 stack (TKeyword "⊔") = case stack of
    (Sreal stack s x0) ->
        gotoPrim s (Sprim stack s (reduction76 x0))
run' S44 stack (TKeyword ",") = case stack of
    (Sreal stack s x0) ->
        gotoPrim s (Sprim stack s (reduction76 x0))
run' S45 stack (TKeyword "-") = case stack of
    (Skeyword stack s) ->
        gotoPrim s (Sprim stack s (reduction74 ))
run' S45 stack (TKeyword "[") = case stack of
    (Skeyword stack s) ->
        gotoPrim s (Sprim stack s (reduction74 ))
run' S45 stack (TKeyword "^") = case stack of
    (Skeyword stack s) ->
        gotoPrim s (Sprim stack s (reduction74 ))
run' S45 stack (TKeyword ")") = case stack of
    (Skeyword stack s) ->
        gotoPrim s (Sprim stack s (reduction74 ))
run' S45 stack (TKeyword "⟦") = case stack of
    (Skeyword stack s) ->
        gotoPrim s (Sprim stack s (reduction74 ))
run' S45 stack (TKeyword "≤") = case stack of
    (Skeyword stack s) ->
        gotoPrim s (Sprim stack s (reduction74 ))
run' S45 stack (TKeyword "<") = case stack of
    (Skeyword stack s) ->
        gotoPrim s (Sprim stack s (reduction74 ))
run' S45 stack (TKeyword "≠") = case stack of
    (Skeyword stack s) ->
        gotoPrim s (Sprim stack s (reduction74 ))
run' S45 stack (TKeyword "{") = case stack of
    (Skeyword stack s) ->
        gotoPrim s (Sprim stack s (reduction74 ))
run' S45 stack (TKeyword "⟮") = case stack of
    (Skeyword stack s) ->
        gotoPrim s (Sprim stack s (reduction74 ))
run' S45 stack (TReal arg) = case stack of
    (Skeyword stack s) ->
        gotoPrim s (Sprim stack s (reduction74 ))
run' S45 stack (TKeyword "⋆") = case stack of
    (Skeyword stack s) ->
        gotoPrim s (Sprim stack s (reduction74 ))
run' S45 stack (TKeyword "*") = case stack of
    (Skeyword stack s) ->
        gotoPrim s (Sprim stack s (reduction74 ))
run' S45 stack (TKeyword "def") = case stack of
    (Skeyword stack s) ->
        gotoPrim s (Sprim stack s (reduction74 ))
run' S45 stack TEnd = case stack of
    (Skeyword stack s) ->
        gotoPrim s (Sprim stack s (reduction74 ))
run' S45 stack (TInteger arg) = case stack of
    (Skeyword stack s) ->
        gotoPrim s (Sprim stack s (reduction74 ))
run' S45 stack (TKeyword "∘") = case stack of
    (Skeyword stack s) ->
        gotoPrim s (Sprim stack s (reduction74 ))
run' S45 stack (TKeyword "⟧") = case stack of
    (Skeyword stack s) ->
        gotoPrim s (Sprim stack s (reduction74 ))
run' S45 stack (TKeyword "≥") = case stack of
    (Skeyword stack s) ->
        gotoPrim s (Sprim stack s (reduction74 ))
run' S45 stack (TKeyword "|") = case stack of
    (Skeyword stack s) ->
        gotoPrim s (Sprim stack s (reduction74 ))
run' S45 stack (TKeyword "=") = case stack of
    (Skeyword stack s) ->
        gotoPrim s (Sprim stack s (reduction74 ))
run' S45 stack (TKeyword "≡") = case stack of
    (Skeyword stack s) ->
        gotoPrim s (Sprim stack s (reduction74 ))
run' S45 stack (TKeyword "⟯") = case stack of
    (Skeyword stack s) ->
        gotoPrim s (Sprim stack s (reduction74 ))
run' S45 stack (TKeyword "→") = case stack of
    (Skeyword stack s) ->
        gotoPrim s (Sprim stack s (reduction74 ))
run' S45 stack (TKeyword "+") = case stack of
    (Skeyword stack s) ->
        gotoPrim s (Sprim stack s (reduction74 ))
run' S45 stack (TKeyword "⟨") = case stack of
    (Skeyword stack s) ->
        gotoPrim s (Sprim stack s (reduction74 ))
run' S45 stack (TKeyword "export") = case stack of
    (Skeyword stack s) ->
        gotoPrim s (Sprim stack s (reduction74 ))
run' S45 stack (TId arg) = case stack of
    (Skeyword stack s) ->
        gotoPrim s (Sprim stack s (reduction74 ))
run' S45 stack (TString arg) = case stack of
    (Skeyword stack s) ->
        gotoPrim s (Sprim stack s (reduction74 ))
run' S45 stack (TKeyword "∧") = case stack of
    (Skeyword stack s) ->
        gotoPrim s (Sprim stack s (reduction74 ))
run' S45 stack (TKeyword ">") = case stack of
    (Skeyword stack s) ->
        gotoPrim s (Sprim stack s (reduction74 ))
run' S45 stack (TKeyword "}") = case stack of
    (Skeyword stack s) ->
        gotoPrim s (Sprim stack s (reduction74 ))
run' S45 stack (TKeyword ":") = case stack of
    (Skeyword stack s) ->
        gotoPrim s (Sprim stack s (reduction74 ))
run' S45 stack (TKeyword "/") = case stack of
    (Skeyword stack s) ->
        gotoPrim s (Sprim stack s (reduction74 ))
run' S45 stack (TKeyword "⊓") = case stack of
    (Skeyword stack s) ->
        gotoPrim s (Sprim stack s (reduction74 ))
run' S45 stack (TKeyword "(") = case stack of
    (Skeyword stack s) ->
        gotoPrim s (Sprim stack s (reduction74 ))
run' S45 stack (TKeyword "import") = case stack of
    (Skeyword stack s) ->
        gotoPrim s (Sprim stack s (reduction74 ))
run' S45 stack (TKeyword "∨") = case stack of
    (Skeyword stack s) ->
        gotoPrim s (Sprim stack s (reduction74 ))
run' S45 stack (TKeyword "⟩") = case stack of
    (Skeyword stack s) ->
        gotoPrim s (Sprim stack s (reduction74 ))
run' S45 stack (TKeyword "]") = case stack of
    (Skeyword stack s) ->
        gotoPrim s (Sprim stack s (reduction74 ))
run' S45 stack (TKeyword ";") = case stack of
    (Skeyword stack s) ->
        gotoPrim s (Sprim stack s (reduction74 ))
run' S45 stack (TKeyword "⊔") = case stack of
    (Skeyword stack s) ->
        gotoPrim s (Sprim stack s (reduction74 ))
run' S45 stack (TKeyword ",") = case stack of
    (Skeyword stack s) ->
        gotoPrim s (Sprim stack s (reduction74 ))
run' S46 stack (TKeyword "⟨") = shift >> run S13 (Skeyword stack S46)
run' S46 stack (TId arg) = gotoStructbind S46 (Sstructbind stack S46 reduction91)
run' S47 stack (TKeyword "import") = case stack of
    (Snormal (Skeyword (Sid (Skeyword stack s) _ x0) _) _ x1) ->
        gotoStatement s (Sstatement stack s (reduction14 x0 x1))
run' S47 stack (TKeyword "def") = case stack of
    (Snormal (Skeyword (Sid (Skeyword stack s) _ x0) _) _ x1) ->
        gotoStatement s (Sstatement stack s (reduction14 x0 x1))
run' S47 stack (TKeyword "export") = case stack of
    (Snormal (Skeyword (Sid (Skeyword stack s) _ x0) _) _ x1) ->
        gotoStatement s (Sstatement stack s (reduction14 x0 x1))
run' S47 stack TEnd = case stack of
    (Snormal (Skeyword (Sid (Skeyword stack s) _ x0) _) _ x1) ->
        gotoStatement s (Sstatement stack s (reduction14 x0 x1))
run' S48 stack (TKeyword ")") = case stack of
    (Scomp stack s x0) ->
        gotoEquals s (Sequals stack s (reduction49 x0))
run' S48 stack (TKeyword "export") = case stack of
    (Scomp stack s x0) ->
        gotoEquals s (Sequals stack s (reduction49 x0))
run' S48 stack (TKeyword "import") = case stack of
    (Scomp stack s x0) ->
        gotoEquals s (Sequals stack s (reduction49 x0))
run' S48 stack (TKeyword "∧") = case stack of
    (Scomp stack s x0) ->
        gotoEquals s (Sequals stack s (reduction49 x0))
run' S48 stack (TKeyword "≤") = shift >> run S103 (Skeyword stack S48)
run' S48 stack (TKeyword ">") = shift >> run S105 (Skeyword stack S48)
run' S48 stack (TKeyword "<") = shift >> run S106 (Skeyword stack S48)
run' S48 stack (TKeyword "}") = case stack of
    (Scomp stack s x0) ->
        gotoEquals s (Sequals stack s (reduction49 x0))
run' S48 stack (TKeyword "≠") = shift >> run S108 (Skeyword stack S48)
run' S48 stack (TKeyword ":") = case stack of
    (Scomp stack s x0) ->
        gotoEquals s (Sequals stack s (reduction49 x0))
run' S48 stack (TKeyword "⊓") = case stack of
    (Scomp stack s x0) ->
        gotoEquals s (Sequals stack s (reduction49 x0))
run' S48 stack (TKeyword "def") = case stack of
    (Scomp stack s x0) ->
        gotoEquals s (Sequals stack s (reduction49 x0))
run' S48 stack (TKeyword "∨") = case stack of
    (Scomp stack s x0) ->
        gotoEquals s (Sequals stack s (reduction49 x0))
run' S48 stack (TKeyword "⟩") = case stack of
    (Scomp stack s x0) ->
        gotoEquals s (Sequals stack s (reduction49 x0))
run' S48 stack (TKeyword "]") = case stack of
    (Scomp stack s x0) ->
        gotoEquals s (Sequals stack s (reduction49 x0))
run' S48 stack TEnd = case stack of
    (Scomp stack s x0) ->
        gotoEquals s (Sequals stack s (reduction49 x0))
run' S48 stack (TKeyword "⟧") = case stack of
    (Scomp stack s x0) ->
        gotoEquals s (Sequals stack s (reduction49 x0))
run' S48 stack (TKeyword "≥") = shift >> run S104 (Skeyword stack S48)
run' S48 stack (TKeyword "|") = case stack of
    (Scomp stack s x0) ->
        gotoEquals s (Sequals stack s (reduction49 x0))
run' S48 stack (TKeyword "=") = shift >> run S107 (Skeyword stack S48)
run' S48 stack (TKeyword "≡") = case stack of
    (Scomp stack s x0) ->
        gotoEquals s (Sequals stack s (reduction49 x0))
run' S48 stack (TKeyword ";") = case stack of
    (Scomp stack s x0) ->
        gotoEquals s (Sequals stack s (reduction49 x0))
run' S48 stack (TKeyword "⊔") = case stack of
    (Scomp stack s x0) ->
        gotoEquals s (Sequals stack s (reduction49 x0))
run' S48 stack (TKeyword "⟯") = case stack of
    (Scomp stack s x0) ->
        gotoEquals s (Sequals stack s (reduction49 x0))
run' S48 stack (TKeyword "→") = case stack of
    (Scomp stack s x0) ->
        gotoEquals s (Sequals stack s (reduction49 x0))
run' S48 stack (TKeyword ",") = case stack of
    (Scomp stack s x0) ->
        gotoEquals s (Sequals stack s (reduction49 x0))
run' S49 stack (TKeyword "-") = shift >> run S28 (Skeyword stack S49)
run' S49 stack (TKeyword "⋆") = shift >> run S45 (Skeyword stack S49)
run' S49 stack (TKeyword "⟨") = shift >> run S31 (Skeyword stack S49)
run' S49 stack (TKeyword "λ") = shift >> run S46 (Skeyword stack S49)
run' S49 stack (TKeyword "(") = shift >> run S49 (Skeyword stack S49)
run' S49 stack (TKeyword "∃") = shift >> run S52 (Skeyword stack S49)
run' S49 stack (TKeyword "[") = shift >> run S54 (Skeyword stack S49)
run' S49 stack (TInteger arg) = shift >> run S57 (Sinteger stack S49 arg)
run' S49 stack (TString arg) = shift >> run S36 (Sstring stack S49 arg)
run' S49 stack (TKeyword "⟦") = shift >> run S37 (Skeyword stack S49)
run' S49 stack (TKeyword "~") = shift >> run S59 (Skeyword stack S49)
run' S49 stack (TId arg) = shift >> run S109 (Sid stack S49 arg)
run' S49 stack (TKeyword "{") = shift >> run S42 (Skeyword stack S49)
run' S49 stack (TReal arg) = shift >> run S44 (Sreal stack S49 arg)
run' S49 stack (TKeyword "⟮") = shift >> run S43 (Skeyword stack S49)
run' S49 stack (TKeyword "¬") = shift >> run S63 (Skeyword stack S49)
run' S50 stack (TKeyword "import") = case stack of
    (Scocase stack s x0) ->
        gotoLet s (Slet stack s (reduction34 x0))
run' S50 stack (TKeyword "⟧") = case stack of
    (Scocase stack s x0) ->
        gotoLet s (Slet stack s (reduction34 x0))
run' S50 stack (TKeyword ";") = case stack of
    (Scocase stack s x0) ->
        gotoLet s (Slet stack s (reduction34 x0))
run' S50 stack (TKeyword "⟩") = case stack of
    (Scocase stack s x0) ->
        gotoLet s (Slet stack s (reduction34 x0))
run' S50 stack (TKeyword "def") = case stack of
    (Scocase stack s x0) ->
        gotoLet s (Slet stack s (reduction34 x0))
run' S50 stack (TKeyword "export") = case stack of
    (Scocase stack s x0) ->
        gotoLet s (Slet stack s (reduction34 x0))
run' S50 stack (TKeyword ")") = case stack of
    (Scocase stack s x0) ->
        gotoLet s (Slet stack s (reduction34 x0))
run' S50 stack (TKeyword "|") = case stack of
    (Scocase stack s x0) ->
        gotoLet s (Slet stack s (reduction34 x0))
run' S50 stack (TKeyword "]") = case stack of
    (Scocase stack s x0) ->
        gotoLet s (Slet stack s (reduction34 x0))
run' S50 stack (TKeyword "}") = case stack of
    (Scocase stack s x0) ->
        gotoLet s (Slet stack s (reduction34 x0))
run' S50 stack (TKeyword "≡") = case stack of
    (Scocase stack s x0) ->
        gotoLet s (Slet stack s (reduction34 x0))
run' S50 stack (TKeyword ":") = case stack of
    (Scocase stack s x0) ->
        gotoLet s (Slet stack s (reduction34 x0))
run' S50 stack TEnd = case stack of
    (Scocase stack s x0) ->
        gotoLet s (Slet stack s (reduction34 x0))
run' S50 stack (TKeyword "⟯") = case stack of
    (Scocase stack s x0) ->
        gotoLet s (Slet stack s (reduction34 x0))
run' S50 stack (TKeyword "→") = case stack of
    (Scocase stack s x0) ->
        gotoLet s (Slet stack s (reduction34 x0))
run' S50 stack (TKeyword ",") = case stack of
    (Scocase stack s x0) ->
        gotoLet s (Slet stack s (reduction34 x0))
run' S51 stack (TKeyword "import") = case stack of
    (Sequals stack s x0) ->
        gotoNot s (Snot stack s (reduction47 x0))
run' S51 stack (TKeyword "∧") = case stack of
    (Sequals stack s x0) ->
        gotoNot s (Snot stack s (reduction47 x0))
run' S51 stack (TKeyword "∨") = case stack of
    (Sequals stack s x0) ->
        gotoNot s (Snot stack s (reduction47 x0))
run' S51 stack (TKeyword ";") = case stack of
    (Sequals stack s x0) ->
        gotoNot s (Snot stack s (reduction47 x0))
run' S51 stack (TKeyword "⟩") = case stack of
    (Sequals stack s x0) ->
        gotoNot s (Snot stack s (reduction47 x0))
run' S51 stack (TKeyword "def") = case stack of
    (Sequals stack s x0) ->
        gotoNot s (Snot stack s (reduction47 x0))
run' S51 stack (TKeyword "export") = case stack of
    (Sequals stack s x0) ->
        gotoNot s (Snot stack s (reduction47 x0))
run' S51 stack (TKeyword ")") = case stack of
    (Sequals stack s x0) ->
        gotoNot s (Snot stack s (reduction47 x0))
run' S51 stack (TKeyword "⟧") = case stack of
    (Sequals stack s x0) ->
        gotoNot s (Snot stack s (reduction47 x0))
run' S51 stack (TKeyword "|") = case stack of
    (Sequals stack s x0) ->
        gotoNot s (Snot stack s (reduction47 x0))
run' S51 stack (TKeyword "]") = case stack of
    (Sequals stack s x0) ->
        gotoNot s (Snot stack s (reduction47 x0))
run' S51 stack (TKeyword "}") = case stack of
    (Sequals stack s x0) ->
        gotoNot s (Snot stack s (reduction47 x0))
run' S51 stack (TKeyword "≡") = case stack of
    (Sequals stack s x0) ->
        gotoNot s (Snot stack s (reduction47 x0))
run' S51 stack (TKeyword ":") = case stack of
    (Sequals stack s x0) ->
        gotoNot s (Snot stack s (reduction47 x0))
run' S51 stack TEnd = case stack of
    (Sequals stack s x0) ->
        gotoNot s (Snot stack s (reduction47 x0))
run' S51 stack (TKeyword "⊔") = case stack of
    (Sequals stack s x0) ->
        gotoNot s (Snot stack s (reduction47 x0))
run' S51 stack (TKeyword "⟯") = case stack of
    (Sequals stack s x0) ->
        gotoNot s (Snot stack s (reduction47 x0))
run' S51 stack (TKeyword "→") = case stack of
    (Sequals stack s x0) ->
        gotoNot s (Snot stack s (reduction47 x0))
run' S51 stack (TKeyword "⊓") = case stack of
    (Sequals stack s x0) ->
        gotoNot s (Snot stack s (reduction47 x0))
run' S51 stack (TKeyword ",") = case stack of
    (Sequals stack s x0) ->
        gotoNot s (Snot stack s (reduction47 x0))
run' S52 stack (TKeyword "◊") = shift >> run S112 (Skeyword stack S52)
run' S52 stack (TId arg) = shift >> run S113 (Sid stack S52 arg)
run' S53 stack (TKeyword "import") = case stack of
    (Slet stack s x0) ->
        gotoMonad s (Smonad stack s (reduction31 x0))
run' S53 stack (TKeyword "⟧") = case stack of
    (Slet stack s x0) ->
        gotoMonad s (Smonad stack s (reduction31 x0))
run' S53 stack (TKeyword ";") = shift >> run S114 (Skeyword stack S53)
run' S53 stack (TKeyword "⟩") = case stack of
    (Slet stack s x0) ->
        gotoMonad s (Smonad stack s (reduction31 x0))
run' S53 stack (TKeyword "def") = case stack of
    (Slet stack s x0) ->
        gotoMonad s (Smonad stack s (reduction31 x0))
run' S53 stack (TKeyword "export") = case stack of
    (Slet stack s x0) ->
        gotoMonad s (Smonad stack s (reduction31 x0))
run' S53 stack (TKeyword ")") = case stack of
    (Slet stack s x0) ->
        gotoMonad s (Smonad stack s (reduction31 x0))
run' S53 stack (TKeyword "|") = case stack of
    (Slet stack s x0) ->
        gotoMonad s (Smonad stack s (reduction31 x0))
run' S53 stack (TKeyword "]") = case stack of
    (Slet stack s x0) ->
        gotoMonad s (Smonad stack s (reduction31 x0))
run' S53 stack (TKeyword "}") = case stack of
    (Slet stack s x0) ->
        gotoMonad s (Smonad stack s (reduction31 x0))
run' S53 stack (TKeyword "≡") = case stack of
    (Slet stack s x0) ->
        gotoMonad s (Smonad stack s (reduction31 x0))
run' S53 stack (TKeyword ":") = case stack of
    (Slet stack s x0) ->
        gotoMonad s (Smonad stack s (reduction31 x0))
run' S53 stack TEnd = case stack of
    (Slet stack s x0) ->
        gotoMonad s (Smonad stack s (reduction31 x0))
run' S53 stack (TKeyword "⟯") = case stack of
    (Slet stack s x0) ->
        gotoMonad s (Smonad stack s (reduction31 x0))
run' S53 stack (TKeyword "→") = case stack of
    (Slet stack s x0) ->
        gotoMonad s (Smonad stack s (reduction31 x0))
run' S53 stack (TKeyword ",") = case stack of
    (Slet stack s x0) ->
        gotoMonad s (Smonad stack s (reduction31 x0))
run' S54 stack (TReal arg) = shift >> run S44 (Sreal stack S54 arg)
run' S54 stack (TKeyword "⋆") = shift >> run S45 (Skeyword stack S54)
run' S54 stack (TKeyword "λ") = shift >> run S46 (Skeyword stack S54)
run' S54 stack (TKeyword "(") = shift >> run S96 (Skeyword stack S54)
run' S54 stack (TKeyword "∃") = shift >> run S52 (Skeyword stack S54)
run' S54 stack (TKeyword "]") = shift >> run S116 (Skeyword stack S54)
run' S54 stack (TKeyword "[") = shift >> run S54 (Skeyword stack S54)
run' S54 stack (TInteger arg) = shift >> run S57 (Sinteger stack S54 arg)
run' S54 stack (TString arg) = shift >> run S36 (Sstring stack S54 arg)
run' S54 stack (TKeyword "~") = shift >> run S59 (Skeyword stack S54)
run' S54 stack (TId arg) = shift >> run S39 (Sid stack S54 arg)
run' S54 stack (TKeyword "{") = shift >> run S42 (Skeyword stack S54)
run' S54 stack (TKeyword "⟮") = shift >> run S43 (Skeyword stack S54)
run' S54 stack (TKeyword "¬") = shift >> run S63 (Skeyword stack S54)
run' S55 stack (TKeyword "-") = shift >> run S117 (Skeyword stack S55)
run' S55 stack (TKeyword "+") = shift >> run S118 (Skeyword stack S55)
run' S55 stack (TKeyword ")") = case stack of
    (Smul stack s x0) ->
        gotoAdd s (Sadd stack s (reduction58 x0))
run' S55 stack (TKeyword "export") = case stack of
    (Smul stack s x0) ->
        gotoAdd s (Sadd stack s (reduction58 x0))
run' S55 stack (TKeyword "import") = case stack of
    (Smul stack s x0) ->
        gotoAdd s (Sadd stack s (reduction58 x0))
run' S55 stack (TKeyword "∧") = case stack of
    (Smul stack s x0) ->
        gotoAdd s (Sadd stack s (reduction58 x0))
run' S55 stack (TKeyword "≤") = case stack of
    (Smul stack s x0) ->
        gotoAdd s (Sadd stack s (reduction58 x0))
run' S55 stack (TKeyword ">") = case stack of
    (Smul stack s x0) ->
        gotoAdd s (Sadd stack s (reduction58 x0))
run' S55 stack (TKeyword "<") = case stack of
    (Smul stack s x0) ->
        gotoAdd s (Sadd stack s (reduction58 x0))
run' S55 stack (TKeyword "}") = case stack of
    (Smul stack s x0) ->
        gotoAdd s (Sadd stack s (reduction58 x0))
run' S55 stack (TKeyword "≠") = case stack of
    (Smul stack s x0) ->
        gotoAdd s (Sadd stack s (reduction58 x0))
run' S55 stack (TKeyword ":") = case stack of
    (Smul stack s x0) ->
        gotoAdd s (Sadd stack s (reduction58 x0))
run' S55 stack (TKeyword "⊓") = case stack of
    (Smul stack s x0) ->
        gotoAdd s (Sadd stack s (reduction58 x0))
run' S55 stack (TKeyword "def") = case stack of
    (Smul stack s x0) ->
        gotoAdd s (Sadd stack s (reduction58 x0))
run' S55 stack (TKeyword "∨") = case stack of
    (Smul stack s x0) ->
        gotoAdd s (Sadd stack s (reduction58 x0))
run' S55 stack (TKeyword "⟩") = case stack of
    (Smul stack s x0) ->
        gotoAdd s (Sadd stack s (reduction58 x0))
run' S55 stack (TKeyword "]") = case stack of
    (Smul stack s x0) ->
        gotoAdd s (Sadd stack s (reduction58 x0))
run' S55 stack TEnd = case stack of
    (Smul stack s x0) ->
        gotoAdd s (Sadd stack s (reduction58 x0))
run' S55 stack (TKeyword "∘") = case stack of
    (Smul stack s x0) ->
        gotoAdd s (Sadd stack s (reduction58 x0))
run' S55 stack (TKeyword "⟧") = case stack of
    (Smul stack s x0) ->
        gotoAdd s (Sadd stack s (reduction58 x0))
run' S55 stack (TKeyword "≥") = case stack of
    (Smul stack s x0) ->
        gotoAdd s (Sadd stack s (reduction58 x0))
run' S55 stack (TKeyword "|") = case stack of
    (Smul stack s x0) ->
        gotoAdd s (Sadd stack s (reduction58 x0))
run' S55 stack (TKeyword "=") = case stack of
    (Smul stack s x0) ->
        gotoAdd s (Sadd stack s (reduction58 x0))
run' S55 stack (TKeyword "≡") = case stack of
    (Smul stack s x0) ->
        gotoAdd s (Sadd stack s (reduction58 x0))
run' S55 stack (TKeyword ";") = case stack of
    (Smul stack s x0) ->
        gotoAdd s (Sadd stack s (reduction58 x0))
run' S55 stack (TKeyword "⊔") = case stack of
    (Smul stack s x0) ->
        gotoAdd s (Sadd stack s (reduction58 x0))
run' S55 stack (TKeyword "⟯") = case stack of
    (Smul stack s x0) ->
        gotoAdd s (Sadd stack s (reduction58 x0))
run' S55 stack (TKeyword "→") = case stack of
    (Smul stack s x0) ->
        gotoAdd s (Sadd stack s (reduction58 x0))
run' S55 stack (TKeyword ",") = case stack of
    (Smul stack s x0) ->
        gotoAdd s (Sadd stack s (reduction58 x0))
run' S56 stack (TKeyword "import") = case stack of
    (Snot stack s x0) ->
        gotoConj s (Sconj stack s (reduction45 x0))
run' S56 stack (TKeyword "∧") = shift >> run S119 (Skeyword stack S56)
run' S56 stack (TKeyword "∨") = case stack of
    (Snot stack s x0) ->
        gotoConj s (Sconj stack s (reduction45 x0))
run' S56 stack (TKeyword ";") = case stack of
    (Snot stack s x0) ->
        gotoConj s (Sconj stack s (reduction45 x0))
run' S56 stack (TKeyword "⟩") = case stack of
    (Snot stack s x0) ->
        gotoConj s (Sconj stack s (reduction45 x0))
run' S56 stack (TKeyword "def") = case stack of
    (Snot stack s x0) ->
        gotoConj s (Sconj stack s (reduction45 x0))
run' S56 stack (TKeyword "export") = case stack of
    (Snot stack s x0) ->
        gotoConj s (Sconj stack s (reduction45 x0))
run' S56 stack (TKeyword ")") = case stack of
    (Snot stack s x0) ->
        gotoConj s (Sconj stack s (reduction45 x0))
run' S56 stack (TKeyword "⟧") = case stack of
    (Snot stack s x0) ->
        gotoConj s (Sconj stack s (reduction45 x0))
run' S56 stack (TKeyword "|") = case stack of
    (Snot stack s x0) ->
        gotoConj s (Sconj stack s (reduction45 x0))
run' S56 stack (TKeyword "]") = case stack of
    (Snot stack s x0) ->
        gotoConj s (Sconj stack s (reduction45 x0))
run' S56 stack (TKeyword "}") = case stack of
    (Snot stack s x0) ->
        gotoConj s (Sconj stack s (reduction45 x0))
run' S56 stack (TKeyword "≡") = case stack of
    (Snot stack s x0) ->
        gotoConj s (Sconj stack s (reduction45 x0))
run' S56 stack (TKeyword ":") = case stack of
    (Snot stack s x0) ->
        gotoConj s (Sconj stack s (reduction45 x0))
run' S56 stack TEnd = case stack of
    (Snot stack s x0) ->
        gotoConj s (Sconj stack s (reduction45 x0))
run' S56 stack (TKeyword "⊔") = case stack of
    (Snot stack s x0) ->
        gotoConj s (Sconj stack s (reduction45 x0))
run' S56 stack (TKeyword "⟯") = case stack of
    (Snot stack s x0) ->
        gotoConj s (Sconj stack s (reduction45 x0))
run' S56 stack (TKeyword "→") = case stack of
    (Snot stack s x0) ->
        gotoConj s (Sconj stack s (reduction45 x0))
run' S56 stack (TKeyword "⊓") = case stack of
    (Snot stack s x0) ->
        gotoConj s (Sconj stack s (reduction45 x0))
run' S56 stack (TKeyword ",") = case stack of
    (Snot stack s x0) ->
        gotoConj s (Sconj stack s (reduction45 x0))
run' S57 stack (TKeyword "-") = case stack of
    (Sinteger stack s x0) ->
        gotoPrim s (Sprim stack s (reduction75 x0))
run' S57 stack (TKeyword "[") = case stack of
    (Sinteger stack s x0) ->
        gotoPrim s (Sprim stack s (reduction75 x0))
run' S57 stack (TKeyword "^") = case stack of
    (Sinteger stack s x0) ->
        gotoPrim s (Sprim stack s (reduction75 x0))
run' S57 stack (TKeyword ")") = case stack of
    (Sinteger stack s x0) ->
        gotoPrim s (Sprim stack s (reduction75 x0))
run' S57 stack (TKeyword "⟦") = case stack of
    (Sinteger stack s x0) ->
        gotoPrim s (Sprim stack s (reduction75 x0))
run' S57 stack (TKeyword "≤") = case stack of
    (Sinteger stack s x0) ->
        gotoPrim s (Sprim stack s (reduction75 x0))
run' S57 stack (TKeyword "<") = case stack of
    (Sinteger stack s x0) ->
        gotoPrim s (Sprim stack s (reduction75 x0))
run' S57 stack (TKeyword "≠") = case stack of
    (Sinteger stack s x0) ->
        gotoPrim s (Sprim stack s (reduction75 x0))
run' S57 stack (TKeyword "{") = case stack of
    (Sinteger stack s x0) ->
        gotoPrim s (Sprim stack s (reduction75 x0))
run' S57 stack (TKeyword "⟮") = case stack of
    (Sinteger stack s x0) ->
        gotoPrim s (Sprim stack s (reduction75 x0))
run' S57 stack (TReal arg) = case stack of
    (Sinteger stack s x0) ->
        gotoPrim s (Sprim stack s (reduction75 x0))
run' S57 stack (TKeyword "⋆") = case stack of
    (Sinteger stack s x0) ->
        gotoPrim s (Sprim stack s (reduction75 x0))
run' S57 stack (TKeyword "*") = case stack of
    (Sinteger stack s x0) ->
        gotoPrim s (Sprim stack s (reduction75 x0))
run' S57 stack (TKeyword "def") = case stack of
    (Sinteger stack s x0) ->
        gotoPrim s (Sprim stack s (reduction75 x0))
run' S57 stack TEnd = case stack of
    (Sinteger stack s x0) ->
        gotoPrim s (Sprim stack s (reduction75 x0))
run' S57 stack (TInteger arg) = case stack of
    (Sinteger stack s x0) ->
        gotoPrim s (Sprim stack s (reduction75 x0))
run' S57 stack (TKeyword "∘") = case stack of
    (Sinteger stack s x0) ->
        gotoPrim s (Sprim stack s (reduction75 x0))
run' S57 stack (TKeyword "⟧") = case stack of
    (Sinteger stack s x0) ->
        gotoPrim s (Sprim stack s (reduction75 x0))
run' S57 stack (TKeyword "≥") = case stack of
    (Sinteger stack s x0) ->
        gotoPrim s (Sprim stack s (reduction75 x0))
run' S57 stack (TKeyword "|") = case stack of
    (Sinteger stack s x0) ->
        gotoPrim s (Sprim stack s (reduction75 x0))
run' S57 stack (TKeyword "=") = case stack of
    (Sinteger stack s x0) ->
        gotoPrim s (Sprim stack s (reduction75 x0))
run' S57 stack (TKeyword "≡") = case stack of
    (Sinteger stack s x0) ->
        gotoPrim s (Sprim stack s (reduction75 x0))
run' S57 stack (TKeyword "⟯") = case stack of
    (Sinteger stack s x0) ->
        gotoPrim s (Sprim stack s (reduction75 x0))
run' S57 stack (TKeyword "→") = case stack of
    (Sinteger stack s x0) ->
        gotoPrim s (Sprim stack s (reduction75 x0))
run' S57 stack (TKeyword "+") = case stack of
    (Sinteger stack s x0) ->
        gotoPrim s (Sprim stack s (reduction75 x0))
run' S57 stack (TKeyword "⟨") = case stack of
    (Sinteger stack s x0) ->
        gotoPrim s (Sprim stack s (reduction75 x0))
run' S57 stack (TKeyword "export") = case stack of
    (Sinteger stack s x0) ->
        gotoPrim s (Sprim stack s (reduction75 x0))
run' S57 stack (TId arg) = case stack of
    (Sinteger stack s x0) ->
        gotoPrim s (Sprim stack s (reduction75 x0))
run' S57 stack (TString arg) = case stack of
    (Sinteger stack s x0) ->
        gotoPrim s (Sprim stack s (reduction75 x0))
run' S57 stack (TKeyword "∧") = case stack of
    (Sinteger stack s x0) ->
        gotoPrim s (Sprim stack s (reduction75 x0))
run' S57 stack (TKeyword ">") = case stack of
    (Sinteger stack s x0) ->
        gotoPrim s (Sprim stack s (reduction75 x0))
run' S57 stack (TKeyword "}") = case stack of
    (Sinteger stack s x0) ->
        gotoPrim s (Sprim stack s (reduction75 x0))
run' S57 stack (TKeyword ":") = case stack of
    (Sinteger stack s x0) ->
        gotoPrim s (Sprim stack s (reduction75 x0))
run' S57 stack (TKeyword "/") = case stack of
    (Sinteger stack s x0) ->
        gotoPrim s (Sprim stack s (reduction75 x0))
run' S57 stack (TKeyword "⊓") = case stack of
    (Sinteger stack s x0) ->
        gotoPrim s (Sprim stack s (reduction75 x0))
run' S57 stack (TKeyword "(") = case stack of
    (Sinteger stack s x0) ->
        gotoPrim s (Sprim stack s (reduction75 x0))
run' S57 stack (TKeyword "import") = case stack of
    (Sinteger stack s x0) ->
        gotoPrim s (Sprim stack s (reduction75 x0))
run' S57 stack (TKeyword "∨") = case stack of
    (Sinteger stack s x0) ->
        gotoPrim s (Sprim stack s (reduction75 x0))
run' S57 stack (TKeyword "⟩") = case stack of
    (Sinteger stack s x0) ->
        gotoPrim s (Sprim stack s (reduction75 x0))
run' S57 stack (TKeyword "]") = case stack of
    (Sinteger stack s x0) ->
        gotoPrim s (Sprim stack s (reduction75 x0))
run' S57 stack (TKeyword ";") = case stack of
    (Sinteger stack s x0) ->
        gotoPrim s (Sprim stack s (reduction75 x0))
run' S57 stack (TKeyword "⊔") = case stack of
    (Sinteger stack s x0) ->
        gotoPrim s (Sprim stack s (reduction75 x0))
run' S57 stack (TKeyword ",") = case stack of
    (Sinteger stack s x0) ->
        gotoPrim s (Sprim stack s (reduction75 x0))
run' S58 stack (TKeyword "export") = case stack of
    (Sterm stack s x0) ->
        gotoNormal s (Snormal stack s (reduction16 x0))
run' S58 stack (TKeyword "⟩") = case stack of
    (Sterm stack s x0) ->
        gotoNormal s (Snormal stack s (reduction16 x0))
run' S58 stack (TKeyword "def") = case stack of
    (Sterm stack s x0) ->
        gotoNormal s (Snormal stack s (reduction16 x0))
run' S58 stack (TKeyword ")") = case stack of
    (Sterm stack s x0) ->
        gotoNormal s (Snormal stack s (reduction16 x0))
run' S58 stack TEnd = case stack of
    (Sterm stack s x0) ->
        gotoNormal s (Snormal stack s (reduction16 x0))
run' S58 stack (TKeyword "import") = case stack of
    (Sterm stack s x0) ->
        gotoNormal s (Snormal stack s (reduction16 x0))
run' S58 stack (TKeyword "⟧") = case stack of
    (Sterm stack s x0) ->
        gotoNormal s (Snormal stack s (reduction16 x0))
run' S58 stack (TKeyword "|") = case stack of
    (Sterm stack s x0) ->
        gotoNormal s (Snormal stack s (reduction16 x0))
run' S58 stack (TKeyword "}") = case stack of
    (Sterm stack s x0) ->
        gotoNormal s (Snormal stack s (reduction16 x0))
run' S58 stack (TKeyword "≡") = shift >> run S120 (Skeyword stack S58)
run' S58 stack (TKeyword ":") = shift >> run S122 (Skeyword stack S58)
run' S58 stack (TKeyword "⟯") = case stack of
    (Sterm stack s x0) ->
        gotoNormal s (Snormal stack s (reduction16 x0))
run' S58 stack (TKeyword "→") = shift >> run S121 (Skeyword stack S58)
run' S58 stack (TKeyword ",") = case stack of
    (Sterm stack s x0) ->
        gotoNormal s (Snormal stack s (reduction16 x0))
run' S59 stack (TReal arg) = shift >> run S44 (Sreal stack S59 arg)
run' S59 stack (TKeyword "⋆") = shift >> run S45 (Skeyword stack S59)
run' S59 stack (TKeyword "{") = shift >> run S42 (Skeyword stack S59)
run' S59 stack (TKeyword "(") = shift >> run S96 (Skeyword stack S59)
run' S59 stack (TKeyword "[") = shift >> run S54 (Skeyword stack S59)
run' S59 stack (TInteger arg) = shift >> run S57 (Sinteger stack S59 arg)
run' S59 stack (TId arg) = shift >> run S124 (Sid stack S59 arg)
run' S59 stack (TString arg) = shift >> run S36 (Sstring stack S59 arg)
run' S59 stack (TKeyword "~") = shift >> run S59 (Skeyword stack S59)
run' S60 stack (TKeyword "import") = case stack of
    (Sconj2 stack s x0) ->
        gotoDisj s (Sdisj stack s (reduction39 x0))
run' S60 stack (TKeyword "⟧") = case stack of
    (Sconj2 stack s x0) ->
        gotoDisj s (Sdisj stack s (reduction39 x0))
run' S60 stack (TKeyword ";") = case stack of
    (Sconj2 stack s x0) ->
        gotoDisj s (Sdisj stack s (reduction39 x0))
run' S60 stack (TKeyword "⟩") = case stack of
    (Sconj2 stack s x0) ->
        gotoDisj s (Sdisj stack s (reduction39 x0))
run' S60 stack (TKeyword "def") = case stack of
    (Sconj2 stack s x0) ->
        gotoDisj s (Sdisj stack s (reduction39 x0))
run' S60 stack (TKeyword "export") = case stack of
    (Sconj2 stack s x0) ->
        gotoDisj s (Sdisj stack s (reduction39 x0))
run' S60 stack (TKeyword ")") = case stack of
    (Sconj2 stack s x0) ->
        gotoDisj s (Sdisj stack s (reduction39 x0))
run' S60 stack (TKeyword "|") = case stack of
    (Sconj2 stack s x0) ->
        gotoDisj s (Sdisj stack s (reduction39 x0))
run' S60 stack (TKeyword "]") = case stack of
    (Sconj2 stack s x0) ->
        gotoDisj s (Sdisj stack s (reduction39 x0))
run' S60 stack (TKeyword "}") = case stack of
    (Sconj2 stack s x0) ->
        gotoDisj s (Sdisj stack s (reduction39 x0))
run' S60 stack (TKeyword "≡") = case stack of
    (Sconj2 stack s x0) ->
        gotoDisj s (Sdisj stack s (reduction39 x0))
run' S60 stack (TKeyword ":") = case stack of
    (Sconj2 stack s x0) ->
        gotoDisj s (Sdisj stack s (reduction39 x0))
run' S60 stack TEnd = case stack of
    (Sconj2 stack s x0) ->
        gotoDisj s (Sdisj stack s (reduction39 x0))
run' S60 stack (TKeyword "⊔") = shift >> run S125 (Skeyword stack S60)
run' S60 stack (TKeyword "⟯") = case stack of
    (Sconj2 stack s x0) ->
        gotoDisj s (Sdisj stack s (reduction39 x0))
run' S60 stack (TKeyword "→") = case stack of
    (Sconj2 stack s x0) ->
        gotoDisj s (Sdisj stack s (reduction39 x0))
run' S60 stack (TKeyword ",") = case stack of
    (Sconj2 stack s x0) ->
        gotoDisj s (Sdisj stack s (reduction39 x0))
run' S61 stack (TKeyword "-") = case stack of
    (Sexp stack s x0) ->
        gotoPrefix s (Sprefix stack s (reduction64 x0))
run' S61 stack (TKeyword "+") = case stack of
    (Sexp stack s x0) ->
        gotoPrefix s (Sprefix stack s (reduction64 x0))
run' S61 stack (TKeyword ")") = case stack of
    (Sexp stack s x0) ->
        gotoPrefix s (Sprefix stack s (reduction64 x0))
run' S61 stack (TKeyword "export") = case stack of
    (Sexp stack s x0) ->
        gotoPrefix s (Sprefix stack s (reduction64 x0))
run' S61 stack (TKeyword "import") = case stack of
    (Sexp stack s x0) ->
        gotoPrefix s (Sprefix stack s (reduction64 x0))
run' S61 stack (TKeyword "∧") = case stack of
    (Sexp stack s x0) ->
        gotoPrefix s (Sprefix stack s (reduction64 x0))
run' S61 stack (TKeyword "≤") = case stack of
    (Sexp stack s x0) ->
        gotoPrefix s (Sprefix stack s (reduction64 x0))
run' S61 stack (TKeyword ">") = case stack of
    (Sexp stack s x0) ->
        gotoPrefix s (Sprefix stack s (reduction64 x0))
run' S61 stack (TKeyword "<") = case stack of
    (Sexp stack s x0) ->
        gotoPrefix s (Sprefix stack s (reduction64 x0))
run' S61 stack (TKeyword "}") = case stack of
    (Sexp stack s x0) ->
        gotoPrefix s (Sprefix stack s (reduction64 x0))
run' S61 stack (TKeyword "≠") = case stack of
    (Sexp stack s x0) ->
        gotoPrefix s (Sprefix stack s (reduction64 x0))
run' S61 stack (TKeyword ":") = case stack of
    (Sexp stack s x0) ->
        gotoPrefix s (Sprefix stack s (reduction64 x0))
run' S61 stack (TKeyword "/") = case stack of
    (Sexp stack s x0) ->
        gotoPrefix s (Sprefix stack s (reduction64 x0))
run' S61 stack (TKeyword "⊓") = case stack of
    (Sexp stack s x0) ->
        gotoPrefix s (Sprefix stack s (reduction64 x0))
run' S61 stack (TKeyword "*") = case stack of
    (Sexp stack s x0) ->
        gotoPrefix s (Sprefix stack s (reduction64 x0))
run' S61 stack (TKeyword "def") = case stack of
    (Sexp stack s x0) ->
        gotoPrefix s (Sprefix stack s (reduction64 x0))
run' S61 stack (TKeyword "∨") = case stack of
    (Sexp stack s x0) ->
        gotoPrefix s (Sprefix stack s (reduction64 x0))
run' S61 stack (TKeyword "⟩") = case stack of
    (Sexp stack s x0) ->
        gotoPrefix s (Sprefix stack s (reduction64 x0))
run' S61 stack (TKeyword "]") = case stack of
    (Sexp stack s x0) ->
        gotoPrefix s (Sprefix stack s (reduction64 x0))
run' S61 stack TEnd = case stack of
    (Sexp stack s x0) ->
        gotoPrefix s (Sprefix stack s (reduction64 x0))
run' S61 stack (TKeyword "∘") = case stack of
    (Sexp stack s x0) ->
        gotoPrefix s (Sprefix stack s (reduction64 x0))
run' S61 stack (TKeyword "⟧") = case stack of
    (Sexp stack s x0) ->
        gotoPrefix s (Sprefix stack s (reduction64 x0))
run' S61 stack (TKeyword "≥") = case stack of
    (Sexp stack s x0) ->
        gotoPrefix s (Sprefix stack s (reduction64 x0))
run' S61 stack (TKeyword "|") = case stack of
    (Sexp stack s x0) ->
        gotoPrefix s (Sprefix stack s (reduction64 x0))
run' S61 stack (TKeyword "=") = case stack of
    (Sexp stack s x0) ->
        gotoPrefix s (Sprefix stack s (reduction64 x0))
run' S61 stack (TKeyword "≡") = case stack of
    (Sexp stack s x0) ->
        gotoPrefix s (Sprefix stack s (reduction64 x0))
run' S61 stack (TKeyword ";") = case stack of
    (Sexp stack s x0) ->
        gotoPrefix s (Sprefix stack s (reduction64 x0))
run' S61 stack (TKeyword "⊔") = case stack of
    (Sexp stack s x0) ->
        gotoPrefix s (Sprefix stack s (reduction64 x0))
run' S61 stack (TKeyword "⟯") = case stack of
    (Sexp stack s x0) ->
        gotoPrefix s (Sprefix stack s (reduction64 x0))
run' S61 stack (TKeyword "→") = case stack of
    (Sexp stack s x0) ->
        gotoPrefix s (Sprefix stack s (reduction64 x0))
run' S61 stack (TKeyword ",") = case stack of
    (Sexp stack s x0) ->
        gotoPrefix s (Sprefix stack s (reduction64 x0))
run' S62 stack (TKeyword "export") = case stack of
    (Slambda stack s x0) ->
        gotoTerm s (Sterm stack s (reduction27 x0))
run' S62 stack (TKeyword "⟩") = case stack of
    (Slambda stack s x0) ->
        gotoTerm s (Sterm stack s (reduction27 x0))
run' S62 stack (TKeyword "def") = case stack of
    (Slambda stack s x0) ->
        gotoTerm s (Sterm stack s (reduction27 x0))
run' S62 stack (TKeyword ")") = case stack of
    (Slambda stack s x0) ->
        gotoTerm s (Sterm stack s (reduction27 x0))
run' S62 stack (TKeyword "]") = case stack of
    (Slambda stack s x0) ->
        gotoTerm s (Sterm stack s (reduction27 x0))
run' S62 stack TEnd = case stack of
    (Slambda stack s x0) ->
        gotoTerm s (Sterm stack s (reduction27 x0))
run' S62 stack (TKeyword "import") = case stack of
    (Slambda stack s x0) ->
        gotoTerm s (Sterm stack s (reduction27 x0))
run' S62 stack (TKeyword "⟧") = case stack of
    (Slambda stack s x0) ->
        gotoTerm s (Sterm stack s (reduction27 x0))
run' S62 stack (TKeyword "|") = case stack of
    (Slambda stack s x0) ->
        gotoTerm s (Sterm stack s (reduction27 x0))
run' S62 stack (TKeyword "}") = case stack of
    (Slambda stack s x0) ->
        gotoTerm s (Sterm stack s (reduction27 x0))
run' S62 stack (TKeyword "≡") = case stack of
    (Slambda stack s x0) ->
        gotoTerm s (Sterm stack s (reduction27 x0))
run' S62 stack (TKeyword ":") = case stack of
    (Slambda stack s x0) ->
        gotoTerm s (Sterm stack s (reduction27 x0))
run' S62 stack (TKeyword "⟯") = case stack of
    (Slambda stack s x0) ->
        gotoTerm s (Sterm stack s (reduction27 x0))
run' S62 stack (TKeyword "→") = case stack of
    (Slambda stack s x0) ->
        gotoTerm s (Sterm stack s (reduction27 x0))
run' S62 stack (TKeyword ",") = case stack of
    (Slambda stack s x0) ->
        gotoTerm s (Sterm stack s (reduction27 x0))
run' S63 stack (TReal arg) = shift >> run S44 (Sreal stack S63 arg)
run' S63 stack (TKeyword "⋆") = shift >> run S45 (Skeyword stack S63)
run' S63 stack (TKeyword "{") = shift >> run S42 (Skeyword stack S63)
run' S63 stack (TInteger arg) = shift >> run S57 (Sinteger stack S63 arg)
run' S63 stack (TKeyword "~") = shift >> run S59 (Skeyword stack S63)
run' S63 stack (TKeyword "(") = shift >> run S96 (Skeyword stack S63)
run' S63 stack (TKeyword "[") = shift >> run S54 (Skeyword stack S63)
run' S63 stack (TId arg) = shift >> run S124 (Sid stack S63 arg)
run' S63 stack (TString arg) = shift >> run S36 (Sstring stack S63 arg)
run' S63 stack (TKeyword "¬") = shift >> run S63 (Skeyword stack S63)
run' S64 stack (TKeyword "import") = case stack of
    (Snormal (Skeyword (Sid (Skeyword stack s) _ x0) _) _ x1) ->
        gotoStatement s (Sstatement stack s (reduction13 x0 x1))
run' S64 stack (TKeyword "def") = case stack of
    (Snormal (Skeyword (Sid (Skeyword stack s) _ x0) _) _ x1) ->
        gotoStatement s (Sstatement stack s (reduction13 x0 x1))
run' S64 stack (TKeyword "export") = case stack of
    (Snormal (Skeyword (Sid (Skeyword stack s) _ x0) _) _ x1) ->
        gotoStatement s (Sstatement stack s (reduction13 x0 x1))
run' S64 stack TEnd = case stack of
    (Snormal (Skeyword (Sid (Skeyword stack s) _ x0) _) _ x1) ->
        gotoStatement s (Sstatement stack s (reduction13 x0 x1))
run' S65 stack (TKeyword "=") = case stack of
    (Slambdabinders (Slambdabinder stack s x0) _ x1) ->
        gotoLambdabinders s (Slambdabinders stack s (reduction117 x0 x1))
run' S65 stack (TKeyword ".") = case stack of
    (Slambdabinders (Slambdabinder stack s x0) _ x1) ->
        gotoLambdabinders s (Slambdabinders stack s (reduction117 x0 x1))
run' S66 stack (TKeyword "import") = case stack of
    (Snames (Skeyword (Sname stack s x0) _) _ x1) ->
        gotoNames s (Snames stack s (reduction7 x0 x1))
run' S66 stack (TKeyword "def") = case stack of
    (Snames (Skeyword (Sname stack s x0) _) _ x1) ->
        gotoNames s (Snames stack s (reduction7 x0 x1))
run' S66 stack (TKeyword "export") = case stack of
    (Snames (Skeyword (Sname stack s x0) _) _ x1) ->
        gotoNames s (Snames stack s (reduction7 x0 x1))
run' S66 stack TEnd = case stack of
    (Snames (Skeyword (Sname stack s x0) _) _ x1) ->
        gotoNames s (Snames stack s (reduction7 x0 x1))
run' S67 stack (TKeyword "import") = case stack of
    (Sid (Skeyword (Sid stack s x0) _) _ x1) ->
        gotoName s (Sname stack s (reduction9 x0 x1))
run' S67 stack (TKeyword "def") = case stack of
    (Sid (Skeyword (Sid stack s x0) _) _ x1) ->
        gotoName s (Sname stack s (reduction9 x0 x1))
run' S67 stack (TKeyword "export") = case stack of
    (Sid (Skeyword (Sid stack s x0) _) _ x1) ->
        gotoName s (Sname stack s (reduction9 x0 x1))
run' S67 stack TEnd = case stack of
    (Sid (Skeyword (Sid stack s x0) _) _ x1) ->
        gotoName s (Sname stack s (reduction9 x0 x1))
run' S67 stack (TKeyword ",") = case stack of
    (Sid (Skeyword (Sid stack s x0) _) _ x1) ->
        gotoName s (Sname stack s (reduction9 x0 x1))
run' S68 stack (TId arg) = case stack of
    (Skeyword (Slabbinds (Skeyword stack s) _ x0) _) ->
        gotoStructbind s (Sstructbind stack s (reduction93 x0))
run' S69 stack (TId arg) = shift >> run S24 (Sid stack S69 arg)
run' S70 stack (TKeyword "import") = case stack of
    (Snormal (Skeyword (Slambdabinders (Sid (Skeyword stack s) _ x0) _ x1) _) _ x2) ->
        gotoStatement s (Sstatement stack s (reduction15 x0 x1 x2))
run' S70 stack (TKeyword "def") = case stack of
    (Snormal (Skeyword (Slambdabinders (Sid (Skeyword stack s) _ x0) _ x1) _) _ x2) ->
        gotoStatement s (Sstatement stack s (reduction15 x0 x1 x2))
run' S70 stack (TKeyword "export") = case stack of
    (Snormal (Skeyword (Slambdabinders (Sid (Skeyword stack s) _ x0) _ x1) _) _ x2) ->
        gotoStatement s (Sstatement stack s (reduction15 x0 x1 x2))
run' S70 stack TEnd = case stack of
    (Snormal (Skeyword (Slambdabinders (Sid (Skeyword stack s) _ x0) _ x1) _) _ x2) ->
        gotoStatement s (Sstatement stack s (reduction15 x0 x1 x2))
run' S71 stack TEnd = case stack of
    (Snormal (Skeyword stack s) _ x0) ->
        gotoNormal s (Snormal stack s (reduction24 x0))
run' S71 stack (TKeyword ")") = case stack of
    (Snormal (Skeyword stack s) _ x0) ->
        gotoNormal s (Snormal stack s (reduction24 x0))
run' S71 stack (TKeyword "def") = case stack of
    (Snormal (Skeyword stack s) _ x0) ->
        gotoNormal s (Snormal stack s (reduction24 x0))
run' S71 stack (TKeyword "⟩") = case stack of
    (Snormal (Skeyword stack s) _ x0) ->
        gotoNormal s (Snormal stack s (reduction24 x0))
run' S71 stack (TKeyword "export") = case stack of
    (Snormal (Skeyword stack s) _ x0) ->
        gotoNormal s (Snormal stack s (reduction24 x0))
run' S71 stack (TKeyword "import") = case stack of
    (Snormal (Skeyword stack s) _ x0) ->
        gotoNormal s (Snormal stack s (reduction24 x0))
run' S71 stack (TKeyword "⟧") = case stack of
    (Snormal (Skeyword stack s) _ x0) ->
        gotoNormal s (Snormal stack s (reduction24 x0))
run' S71 stack (TKeyword "|") = case stack of
    (Snormal (Skeyword stack s) _ x0) ->
        gotoNormal s (Snormal stack s (reduction24 x0))
run' S71 stack (TKeyword "}") = case stack of
    (Snormal (Skeyword stack s) _ x0) ->
        gotoNormal s (Snormal stack s (reduction24 x0))
run' S71 stack (TKeyword "⟯") = case stack of
    (Snormal (Skeyword stack s) _ x0) ->
        gotoNormal s (Snormal stack s (reduction24 x0))
run' S71 stack (TKeyword ",") = case stack of
    (Snormal (Skeyword stack s) _ x0) ->
        gotoNormal s (Snormal stack s (reduction24 x0))
run' S72 stack (TKeyword "⟩") = shift >> run S128 (Skeyword stack S72)
run' S73 stack (TKeyword "⟩") = case stack of
    (Slabtype stack s x0) ->
        gotoLabtypes s (Slabtypes stack s (reduction97 x0))
run' S73 stack (TKeyword ",") = shift >> run S129 (Skeyword stack S73)
run' S74 stack (TReal arg) = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoStructdef s (Sstructdef stack s (reduction89 ))
run' S74 stack (TKeyword "⋆") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoStructdef s (Sstructdef stack s (reduction89 ))
run' S74 stack (TKeyword "λ") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoStructdef s (Sstructdef stack s (reduction89 ))
run' S74 stack (TKeyword "(") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoStructdef s (Sstructdef stack s (reduction89 ))
run' S74 stack (TKeyword "∃") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoStructdef s (Sstructdef stack s (reduction89 ))
run' S74 stack (TKeyword "[") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoStructdef s (Sstructdef stack s (reduction89 ))
run' S74 stack (TInteger arg) = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoStructdef s (Sstructdef stack s (reduction89 ))
run' S74 stack (TId arg) = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoStructdef s (Sstructdef stack s (reduction89 ))
run' S74 stack (TString arg) = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoStructdef s (Sstructdef stack s (reduction89 ))
run' S74 stack (TKeyword "~") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoStructdef s (Sstructdef stack s (reduction89 ))
run' S74 stack (TKeyword "{") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoStructdef s (Sstructdef stack s (reduction89 ))
run' S74 stack (TKeyword "⟮") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoStructdef s (Sstructdef stack s (reduction89 ))
run' S74 stack (TKeyword "¬") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoStructdef s (Sstructdef stack s (reduction89 ))
run' S75 stack (TKeyword ":") = shift >> run S130 (Skeyword stack S75)
run' S76 stack (TId arg) = shift >> run S124 (Sid stack S76 arg)
run' S76 stack (TKeyword "{") = shift >> run S42 (Skeyword stack S76)
run' S76 stack (TReal arg) = shift >> run S44 (Sreal stack S76 arg)
run' S76 stack (TKeyword "⋆") = shift >> run S45 (Skeyword stack S76)
run' S76 stack (TString arg) = shift >> run S36 (Sstring stack S76 arg)
run' S76 stack (TKeyword "(") = shift >> run S96 (Skeyword stack S76)
run' S76 stack (TKeyword "[") = shift >> run S54 (Skeyword stack S76)
run' S76 stack (TInteger arg) = shift >> run S57 (Sinteger stack S76 arg)
run' S76 stack (TKeyword "~") = shift >> run S59 (Skeyword stack S76)
run' S76 stack (TKeyword "¬") = shift >> run S63 (Skeyword stack S76)
run' S77 stack (TReal arg) = shift >> run S44 (Sreal stack S77 arg)
run' S77 stack (TKeyword "⋆") = shift >> run S45 (Skeyword stack S77)
run' S77 stack (TKeyword "{") = shift >> run S42 (Skeyword stack S77)
run' S77 stack (TKeyword "(") = shift >> run S96 (Skeyword stack S77)
run' S77 stack (TKeyword "[") = shift >> run S54 (Skeyword stack S77)
run' S77 stack (TInteger arg) = shift >> run S57 (Sinteger stack S77 arg)
run' S77 stack (TId arg) = shift >> run S124 (Sid stack S77 arg)
run' S77 stack (TString arg) = shift >> run S36 (Sstring stack S77 arg)
run' S77 stack (TKeyword "~") = shift >> run S59 (Skeyword stack S77)
run' S78 stack (TReal arg) = shift >> run S44 (Sreal stack S78 arg)
run' S78 stack (TKeyword "⋆") = shift >> run S45 (Skeyword stack S78)
run' S78 stack (TKeyword "{") = shift >> run S42 (Skeyword stack S78)
run' S78 stack (TKeyword "(") = shift >> run S96 (Skeyword stack S78)
run' S78 stack (TKeyword "[") = shift >> run S54 (Skeyword stack S78)
run' S78 stack (TInteger arg) = shift >> run S57 (Sinteger stack S78 arg)
run' S78 stack (TId arg) = shift >> run S124 (Sid stack S78 arg)
run' S78 stack (TString arg) = shift >> run S36 (Sstring stack S78 arg)
run' S78 stack (TKeyword "~") = shift >> run S59 (Skeyword stack S78)
run' S79 stack (TReal arg) = shift >> run S44 (Sreal stack S79 arg)
run' S79 stack (TKeyword "⋆") = shift >> run S45 (Skeyword stack S79)
run' S79 stack (TKeyword "{") = shift >> run S42 (Skeyword stack S79)
run' S79 stack (TInteger arg) = shift >> run S57 (Sinteger stack S79 arg)
run' S79 stack (TKeyword "~") = shift >> run S59 (Skeyword stack S79)
run' S79 stack (TKeyword "(") = shift >> run S96 (Skeyword stack S79)
run' S79 stack (TKeyword "[") = shift >> run S54 (Skeyword stack S79)
run' S79 stack (TId arg) = shift >> run S124 (Sid stack S79 arg)
run' S79 stack (TString arg) = shift >> run S36 (Sstring stack S79 arg)
run' S79 stack (TKeyword "¬") = shift >> run S63 (Skeyword stack S79)
run' S80 stack (TKeyword "⟧") = shift >> run S135 (Skeyword stack S80)
run' S81 stack (TKeyword "-") = shift >> run S28 (Skeyword stack S81)
run' S81 stack (TKeyword "⋆") = shift >> run S45 (Skeyword stack S81)
run' S81 stack (TKeyword "⟨") = shift >> run S31 (Skeyword stack S81)
run' S81 stack (TKeyword "λ") = shift >> run S46 (Skeyword stack S81)
run' S81 stack (TKeyword "(") = shift >> run S49 (Skeyword stack S81)
run' S81 stack (TKeyword "∃") = shift >> run S52 (Skeyword stack S81)
run' S81 stack (TKeyword "[") = shift >> run S54 (Skeyword stack S81)
run' S81 stack (TInteger arg) = shift >> run S57 (Sinteger stack S81 arg)
run' S81 stack (TString arg) = shift >> run S36 (Sstring stack S81 arg)
run' S81 stack (TKeyword "⟦") = shift >> run S37 (Skeyword stack S81)
run' S81 stack (TKeyword "~") = shift >> run S59 (Skeyword stack S81)
run' S81 stack (TId arg) = shift >> run S136 (Sid stack S81 arg)
run' S81 stack (TKeyword "{") = shift >> run S42 (Skeyword stack S81)
run' S81 stack (TReal arg) = shift >> run S44 (Sreal stack S81 arg)
run' S81 stack (TKeyword "⟮") = shift >> run S43 (Skeyword stack S81)
run' S81 stack (TKeyword "¬") = shift >> run S63 (Skeyword stack S81)
run' S82 stack (TKeyword "→") = shift >> run S137 (Skeyword stack S82)
run' S83 stack (TId arg) = shift >> run S138 (Sid stack S83 arg)
run' S83 stack (TKeyword "{") = shift >> run S42 (Skeyword stack S83)
run' S83 stack (TReal arg) = shift >> run S44 (Sreal stack S83 arg)
run' S83 stack (TKeyword "⋆") = shift >> run S45 (Skeyword stack S83)
run' S83 stack (TString arg) = shift >> run S36 (Sstring stack S83 arg)
run' S83 stack (TKeyword "(") = shift >> run S96 (Skeyword stack S83)
run' S83 stack (TKeyword "[") = shift >> run S54 (Skeyword stack S83)
run' S83 stack (TKeyword "⟮") = shift >> run S43 (Skeyword stack S83)
run' S83 stack (TInteger arg) = shift >> run S57 (Sinteger stack S83 arg)
run' S83 stack (TKeyword "~") = shift >> run S59 (Skeyword stack S83)
run' S83 stack (TKeyword "¬") = shift >> run S63 (Skeyword stack S83)
run' S84 stack (TId arg) = shift >> run S138 (Sid stack S84 arg)
run' S84 stack (TKeyword "{") = shift >> run S42 (Skeyword stack S84)
run' S84 stack (TReal arg) = shift >> run S44 (Sreal stack S84 arg)
run' S84 stack (TKeyword "⋆") = shift >> run S45 (Skeyword stack S84)
run' S84 stack (TString arg) = shift >> run S36 (Sstring stack S84 arg)
run' S84 stack (TKeyword "(") = shift >> run S96 (Skeyword stack S84)
run' S84 stack (TKeyword "[") = shift >> run S54 (Skeyword stack S84)
run' S84 stack (TKeyword "⟮") = shift >> run S43 (Skeyword stack S84)
run' S84 stack (TInteger arg) = shift >> run S57 (Sinteger stack S84 arg)
run' S84 stack (TKeyword "~") = shift >> run S59 (Skeyword stack S84)
run' S84 stack (TKeyword "¬") = shift >> run S63 (Skeyword stack S84)
run' S85 stack (TId arg) = shift >> run S138 (Sid stack S85 arg)
run' S85 stack (TKeyword "{") = shift >> run S42 (Skeyword stack S85)
run' S85 stack (TReal arg) = shift >> run S44 (Sreal stack S85 arg)
run' S85 stack (TKeyword "⋆") = shift >> run S45 (Skeyword stack S85)
run' S85 stack (TString arg) = shift >> run S36 (Sstring stack S85 arg)
run' S85 stack (TKeyword "(") = shift >> run S96 (Skeyword stack S85)
run' S85 stack (TKeyword "[") = shift >> run S54 (Skeyword stack S85)
run' S85 stack (TKeyword "⟮") = shift >> run S43 (Skeyword stack S85)
run' S85 stack (TInteger arg) = shift >> run S57 (Sinteger stack S85 arg)
run' S85 stack (TKeyword "~") = shift >> run S59 (Skeyword stack S85)
run' S85 stack (TKeyword "¬") = shift >> run S63 (Skeyword stack S85)
run' S86 stack (TReal arg) = shift >> run S44 (Sreal stack S86 arg)
run' S86 stack (TKeyword "⋆") = shift >> run S45 (Skeyword stack S86)
run' S86 stack (TKeyword "{") = shift >> run S42 (Skeyword stack S86)
run' S86 stack (TInteger arg) = shift >> run S57 (Sinteger stack S86 arg)
run' S86 stack (TKeyword "~") = shift >> run S59 (Skeyword stack S86)
run' S86 stack (TKeyword "(") = shift >> run S96 (Skeyword stack S86)
run' S86 stack (TKeyword "[") = shift >> run S54 (Skeyword stack S86)
run' S86 stack (TId arg) = shift >> run S124 (Sid stack S86 arg)
run' S86 stack (TString arg) = shift >> run S36 (Sstring stack S86 arg)
run' S87 stack (TKeyword "-") = shift >> run S28 (Skeyword stack S87)
run' S87 stack (TKeyword "⋆") = shift >> run S45 (Skeyword stack S87)
run' S87 stack (TKeyword "⟨") = shift >> run S31 (Skeyword stack S87)
run' S87 stack (TKeyword "λ") = shift >> run S46 (Skeyword stack S87)
run' S87 stack (TKeyword "(") = shift >> run S49 (Skeyword stack S87)
run' S87 stack (TKeyword "∃") = shift >> run S52 (Skeyword stack S87)
run' S87 stack (TKeyword "[") = shift >> run S54 (Skeyword stack S87)
run' S87 stack (TInteger arg) = shift >> run S57 (Sinteger stack S87 arg)
run' S87 stack (TString arg) = shift >> run S36 (Sstring stack S87 arg)
run' S87 stack (TKeyword "⟦") = shift >> run S37 (Skeyword stack S87)
run' S87 stack (TKeyword "~") = shift >> run S59 (Skeyword stack S87)
run' S87 stack (TId arg) = shift >> run S39 (Sid stack S87 arg)
run' S87 stack (TKeyword "{") = shift >> run S42 (Skeyword stack S87)
run' S87 stack (TReal arg) = shift >> run S44 (Sreal stack S87 arg)
run' S87 stack (TKeyword "⟮") = shift >> run S43 (Skeyword stack S87)
run' S87 stack (TKeyword "¬") = shift >> run S63 (Skeyword stack S87)
run' S88 stack (TReal arg) = shift >> run S44 (Sreal stack S88 arg)
run' S88 stack (TKeyword "⋆") = shift >> run S45 (Skeyword stack S88)
run' S88 stack (TKeyword "{") = shift >> run S42 (Skeyword stack S88)
run' S88 stack (TKeyword "[") = shift >> run S54 (Skeyword stack S88)
run' S88 stack (TInteger arg) = shift >> run S57 (Sinteger stack S88 arg)
run' S88 stack (TKeyword "(") = shift >> run S96 (Skeyword stack S88)
run' S88 stack (TId arg) = shift >> run S124 (Sid stack S88 arg)
run' S88 stack (TString arg) = shift >> run S36 (Sstring stack S88 arg)
run' S89 stack (TReal arg) = shift >> run S44 (Sreal stack S89 arg)
run' S89 stack (TKeyword "⋆") = shift >> run S45 (Skeyword stack S89)
run' S89 stack (TKeyword "(") = shift >> run S96 (Skeyword stack S89)
run' S89 stack (TKeyword "[") = shift >> run S54 (Skeyword stack S89)
run' S89 stack (TInteger arg) = shift >> run S57 (Sinteger stack S89 arg)
run' S89 stack (TId arg) = shift >> run S124 (Sid stack S89 arg)
run' S89 stack (TString arg) = shift >> run S36 (Sstring stack S89 arg)
run' S89 stack (TKeyword "{") = shift >> run S42 (Skeyword stack S89)
run' S90 stack (TReal arg) = shift >> run S44 (Sreal stack S90 arg)
run' S90 stack (TKeyword "⋆") = shift >> run S45 (Skeyword stack S90)
run' S90 stack (TKeyword "λ") = shift >> run S46 (Skeyword stack S90)
run' S90 stack (TKeyword "(") = shift >> run S96 (Skeyword stack S90)
run' S90 stack (TKeyword "⟩") = shift >> run S147 (Skeyword stack S90)
run' S90 stack (TKeyword "∃") = shift >> run S52 (Skeyword stack S90)
run' S90 stack (TKeyword "[") = shift >> run S54 (Skeyword stack S90)
run' S90 stack (TInteger arg) = shift >> run S57 (Sinteger stack S90 arg)
run' S90 stack (TString arg) = shift >> run S36 (Sstring stack S90 arg)
run' S90 stack (TKeyword "~") = shift >> run S59 (Skeyword stack S90)
run' S90 stack (TId arg) = shift >> run S93 (Sid stack S90 arg)
run' S90 stack (TKeyword "{") = shift >> run S42 (Skeyword stack S90)
run' S90 stack (TKeyword "⟮") = shift >> run S43 (Skeyword stack S90)
run' S90 stack (TKeyword "¬") = shift >> run S63 (Skeyword stack S90)
run' S91 stack (TId arg) = shift >> run S101 (Sid stack S91 arg)
run' S92 stack (TKeyword "}") = shift >> run S150 (Skeyword stack S92)
run' S92 stack (TKeyword "|") = shift >> run S151 (Skeyword stack S92)
run' S93 stack (TReal arg) = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S93 stack (TKeyword "↦") = shift >> run S83 (Skeyword stack S93)
run' S93 stack (TKeyword "←") = shift >> run S84 (Skeyword stack S93)
run' S93 stack (TKeyword "{") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S93 stack (TKeyword "⟨") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S93 stack (TKeyword "⋆") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S93 stack (TKeyword "^") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S93 stack (TKeyword "(") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S93 stack (TKeyword "=") = shift >> run S152 (Skeyword stack S93)
run' S93 stack (TKeyword "⟦") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S93 stack (TKeyword "[") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S93 stack (TInteger arg) = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S93 stack (TKeyword "⟮") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S93 stack (TKeyword "is") = shift >> run S85 (Skeyword stack S93)
run' S93 stack (TId arg) = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S93 stack (TString arg) = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S94 stack (TKeyword "-") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction78 ))
run' S94 stack (TKeyword "[") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction78 ))
run' S94 stack (TKeyword "^") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction78 ))
run' S94 stack (TKeyword ")") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction78 ))
run' S94 stack (TKeyword "⟦") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction78 ))
run' S94 stack (TKeyword "≤") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction78 ))
run' S94 stack (TKeyword "<") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction78 ))
run' S94 stack (TKeyword "≠") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction78 ))
run' S94 stack (TKeyword "{") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction78 ))
run' S94 stack (TKeyword "⟮") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction78 ))
run' S94 stack (TReal arg) = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction78 ))
run' S94 stack (TKeyword "⋆") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction78 ))
run' S94 stack (TKeyword "*") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction78 ))
run' S94 stack (TKeyword "def") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction78 ))
run' S94 stack TEnd = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction78 ))
run' S94 stack (TInteger arg) = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction78 ))
run' S94 stack (TKeyword "∘") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction78 ))
run' S94 stack (TKeyword "⟧") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction78 ))
run' S94 stack (TKeyword "≥") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction78 ))
run' S94 stack (TKeyword "|") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction78 ))
run' S94 stack (TKeyword "=") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction78 ))
run' S94 stack (TKeyword "≡") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction78 ))
run' S94 stack (TKeyword "⟯") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction78 ))
run' S94 stack (TKeyword "→") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction78 ))
run' S94 stack (TKeyword "+") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction78 ))
run' S94 stack (TKeyword "⟨") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction78 ))
run' S94 stack (TKeyword "export") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction78 ))
run' S94 stack (TId arg) = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction78 ))
run' S94 stack (TString arg) = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction78 ))
run' S94 stack (TKeyword "∧") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction78 ))
run' S94 stack (TKeyword ">") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction78 ))
run' S94 stack (TKeyword "}") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction78 ))
run' S94 stack (TKeyword ":") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction78 ))
run' S94 stack (TKeyword "/") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction78 ))
run' S94 stack (TKeyword "⊓") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction78 ))
run' S94 stack (TKeyword "(") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction78 ))
run' S94 stack (TKeyword "import") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction78 ))
run' S94 stack (TKeyword "∨") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction78 ))
run' S94 stack (TKeyword "⟩") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction78 ))
run' S94 stack (TKeyword "]") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction78 ))
run' S94 stack (TKeyword ";") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction78 ))
run' S94 stack (TKeyword "⊔") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction78 ))
run' S94 stack (TKeyword ",") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction78 ))
run' S95 stack (TKeyword "}") = shift >> run S153 (Skeyword stack S95)
run' S96 stack (TKeyword "-") = shift >> run S28 (Skeyword stack S96)
run' S96 stack (TKeyword "⋆") = shift >> run S45 (Skeyword stack S96)
run' S96 stack (TKeyword "⟨") = shift >> run S31 (Skeyword stack S96)
run' S96 stack (TKeyword "λ") = shift >> run S46 (Skeyword stack S96)
run' S96 stack (TKeyword "(") = shift >> run S49 (Skeyword stack S96)
run' S96 stack (TKeyword "∃") = shift >> run S52 (Skeyword stack S96)
run' S96 stack (TKeyword "[") = shift >> run S54 (Skeyword stack S96)
run' S96 stack (TInteger arg) = shift >> run S57 (Sinteger stack S96 arg)
run' S96 stack (TString arg) = shift >> run S36 (Sstring stack S96 arg)
run' S96 stack (TKeyword "⟦") = shift >> run S37 (Skeyword stack S96)
run' S96 stack (TKeyword "~") = shift >> run S59 (Skeyword stack S96)
run' S96 stack (TId arg) = shift >> run S39 (Sid stack S96 arg)
run' S96 stack (TKeyword "{") = shift >> run S42 (Skeyword stack S96)
run' S96 stack (TReal arg) = shift >> run S44 (Sreal stack S96 arg)
run' S96 stack (TKeyword "⟮") = shift >> run S43 (Skeyword stack S96)
run' S96 stack (TKeyword "¬") = shift >> run S63 (Skeyword stack S96)
run' S97 stack (TKeyword "}") = case stack of
    (Slabterm stack s x0) ->
        gotoLabterms s (Slabterms stack s (reduction94 x0))
run' S97 stack (TKeyword "|") = case stack of
    (Slabterm stack s x0) ->
        gotoLabterms s (Slabterms stack s (reduction94 x0))
run' S97 stack (TKeyword "⟩") = case stack of
    (Slabterm stack s x0) ->
        gotoLabterms s (Slabterms stack s (reduction94 x0))
run' S97 stack (TKeyword ",") = shift >> run S154 (Skeyword stack S97)
run' S98 stack (TKeyword "]") = case stack of
    (Sterm stack s x0) ->
        gotoTerms s (Sterms stack s (reduction25 x0))
run' S98 stack (TKeyword "}") = case stack of
    (Sterm stack s x0) ->
        gotoTerms s (Sterms stack s (reduction25 x0))
run' S98 stack (TKeyword "|") = case stack of
    (Sterm stack s x0) ->
        gotoTerms s (Sterms stack s (reduction25 x0))
run' S98 stack (TKeyword "⟩") = case stack of
    (Sterm stack s x0) ->
        gotoTerms s (Sterms stack s (reduction25 x0))
run' S98 stack (TKeyword ",") = shift >> run S155 (Skeyword stack S98)
run' S99 stack (TKeyword "⟯") = case stack of
    (Scase stack s x0) ->
        gotoCases s (Scases stack s (reduction103 x0))
run' S99 stack (TKeyword ",") = shift >> run S156 (Skeyword stack S99)
run' S100 stack (TKeyword "⟯") = shift >> run S157 (Skeyword stack S100)
run' S101 stack (TId arg) = shift >> run S113 (Sid stack S101 arg)
run' S101 stack (TKeyword "⇒") = gotoBinders0 S101 (Sbinders0 stack S101 reduction106)
run' S101 stack (TKeyword "◊") = shift >> run S112 (Skeyword stack S101)
run' S102 stack (TKeyword ".") = shift >> run S160 (Skeyword stack S102)
run' S103 stack (TReal arg) = shift >> run S44 (Sreal stack S103 arg)
run' S103 stack (TKeyword "⋆") = shift >> run S45 (Skeyword stack S103)
run' S103 stack (TKeyword "{") = shift >> run S42 (Skeyword stack S103)
run' S103 stack (TInteger arg) = shift >> run S57 (Sinteger stack S103 arg)
run' S103 stack (TKeyword "~") = shift >> run S59 (Skeyword stack S103)
run' S103 stack (TKeyword "(") = shift >> run S96 (Skeyword stack S103)
run' S103 stack (TKeyword "[") = shift >> run S54 (Skeyword stack S103)
run' S103 stack (TId arg) = shift >> run S124 (Sid stack S103 arg)
run' S103 stack (TString arg) = shift >> run S36 (Sstring stack S103 arg)
run' S104 stack (TReal arg) = shift >> run S44 (Sreal stack S104 arg)
run' S104 stack (TKeyword "⋆") = shift >> run S45 (Skeyword stack S104)
run' S104 stack (TKeyword "{") = shift >> run S42 (Skeyword stack S104)
run' S104 stack (TInteger arg) = shift >> run S57 (Sinteger stack S104 arg)
run' S104 stack (TKeyword "~") = shift >> run S59 (Skeyword stack S104)
run' S104 stack (TKeyword "(") = shift >> run S96 (Skeyword stack S104)
run' S104 stack (TKeyword "[") = shift >> run S54 (Skeyword stack S104)
run' S104 stack (TId arg) = shift >> run S124 (Sid stack S104 arg)
run' S104 stack (TString arg) = shift >> run S36 (Sstring stack S104 arg)
run' S105 stack (TReal arg) = shift >> run S44 (Sreal stack S105 arg)
run' S105 stack (TKeyword "⋆") = shift >> run S45 (Skeyword stack S105)
run' S105 stack (TKeyword "{") = shift >> run S42 (Skeyword stack S105)
run' S105 stack (TInteger arg) = shift >> run S57 (Sinteger stack S105 arg)
run' S105 stack (TKeyword "~") = shift >> run S59 (Skeyword stack S105)
run' S105 stack (TKeyword "(") = shift >> run S96 (Skeyword stack S105)
run' S105 stack (TKeyword "[") = shift >> run S54 (Skeyword stack S105)
run' S105 stack (TId arg) = shift >> run S124 (Sid stack S105 arg)
run' S105 stack (TString arg) = shift >> run S36 (Sstring stack S105 arg)
run' S106 stack (TReal arg) = shift >> run S44 (Sreal stack S106 arg)
run' S106 stack (TKeyword "⋆") = shift >> run S45 (Skeyword stack S106)
run' S106 stack (TKeyword "{") = shift >> run S42 (Skeyword stack S106)
run' S106 stack (TInteger arg) = shift >> run S57 (Sinteger stack S106 arg)
run' S106 stack (TKeyword "~") = shift >> run S59 (Skeyword stack S106)
run' S106 stack (TKeyword "(") = shift >> run S96 (Skeyword stack S106)
run' S106 stack (TKeyword "[") = shift >> run S54 (Skeyword stack S106)
run' S106 stack (TId arg) = shift >> run S124 (Sid stack S106 arg)
run' S106 stack (TString arg) = shift >> run S36 (Sstring stack S106 arg)
run' S107 stack (TReal arg) = shift >> run S44 (Sreal stack S107 arg)
run' S107 stack (TKeyword "⋆") = shift >> run S45 (Skeyword stack S107)
run' S107 stack (TKeyword "{") = shift >> run S42 (Skeyword stack S107)
run' S107 stack (TInteger arg) = shift >> run S57 (Sinteger stack S107 arg)
run' S107 stack (TKeyword "~") = shift >> run S59 (Skeyword stack S107)
run' S107 stack (TKeyword "(") = shift >> run S96 (Skeyword stack S107)
run' S107 stack (TKeyword "[") = shift >> run S54 (Skeyword stack S107)
run' S107 stack (TId arg) = shift >> run S124 (Sid stack S107 arg)
run' S107 stack (TString arg) = shift >> run S36 (Sstring stack S107 arg)
run' S108 stack (TReal arg) = shift >> run S44 (Sreal stack S108 arg)
run' S108 stack (TKeyword "⋆") = shift >> run S45 (Skeyword stack S108)
run' S108 stack (TKeyword "{") = shift >> run S42 (Skeyword stack S108)
run' S108 stack (TInteger arg) = shift >> run S57 (Sinteger stack S108 arg)
run' S108 stack (TKeyword "~") = shift >> run S59 (Skeyword stack S108)
run' S108 stack (TKeyword "(") = shift >> run S96 (Skeyword stack S108)
run' S108 stack (TKeyword "[") = shift >> run S54 (Skeyword stack S108)
run' S108 stack (TId arg) = shift >> run S124 (Sid stack S108 arg)
run' S108 stack (TString arg) = shift >> run S36 (Sstring stack S108 arg)
run' S109 stack (TReal arg) = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S109 stack (TKeyword "↦") = shift >> run S83 (Skeyword stack S109)
run' S109 stack (TKeyword "←") = shift >> run S84 (Skeyword stack S109)
run' S109 stack (TKeyword "{") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S109 stack (TKeyword "⟨") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S109 stack (TKeyword "⋆") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S109 stack (TKeyword "^") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S109 stack (TKeyword "(") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S109 stack (TKeyword ":") = shift >> run S167 (Skeyword stack S109)
run' S109 stack (TKeyword "[") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S109 stack (TInteger arg) = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S109 stack (TKeyword "⟮") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S109 stack (TKeyword "⟦") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S109 stack (TKeyword "is") = shift >> run S85 (Skeyword stack S109)
run' S109 stack (TId arg) = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S109 stack (TString arg) = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S110 stack (TKeyword ")") = shift >> run S168 (Skeyword stack S110)
run' S111 stack (TKeyword ".") = shift >> run S169 (Skeyword stack S111)
run' S112 stack (TId arg) = shift >> run S170 (Sid stack S112 arg)
run' S113 stack (TKeyword ".") = case stack of
    (Sid stack s x0) ->
        gotoBinders s (Sbinders stack s (reduction108 x0))
run' S113 stack (TId arg) = shift >> run S113 (Sid stack S113 arg)
run' S113 stack (TKeyword "⇒") = case stack of
    (Sid stack s x0) ->
        gotoBinders s (Sbinders stack s (reduction108 x0))
run' S113 stack (TKeyword "◊") = shift >> run S112 (Skeyword stack S113)
run' S114 stack (TId arg) = shift >> run S39 (Sid stack S114 arg)
run' S114 stack (TKeyword "{") = shift >> run S42 (Skeyword stack S114)
run' S114 stack (TReal arg) = shift >> run S44 (Sreal stack S114 arg)
run' S114 stack (TKeyword "⋆") = shift >> run S45 (Skeyword stack S114)
run' S114 stack (TString arg) = shift >> run S36 (Sstring stack S114 arg)
run' S114 stack (TKeyword "(") = shift >> run S96 (Skeyword stack S114)
run' S114 stack (TKeyword "[") = shift >> run S54 (Skeyword stack S114)
run' S114 stack (TKeyword "⟮") = shift >> run S43 (Skeyword stack S114)
run' S114 stack (TInteger arg) = shift >> run S57 (Sinteger stack S114 arg)
run' S114 stack (TKeyword "~") = shift >> run S59 (Skeyword stack S114)
run' S114 stack (TKeyword "¬") = shift >> run S63 (Skeyword stack S114)
run' S115 stack (TKeyword "]") = shift >> run S173 (Skeyword stack S115)
run' S115 stack (TKeyword "|") = shift >> run S174 (Skeyword stack S115)
run' S116 stack (TKeyword "-") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction82 ))
run' S116 stack (TKeyword "[") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction82 ))
run' S116 stack (TKeyword "^") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction82 ))
run' S116 stack (TKeyword ")") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction82 ))
run' S116 stack (TKeyword "⟦") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction82 ))
run' S116 stack (TKeyword "≤") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction82 ))
run' S116 stack (TKeyword "<") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction82 ))
run' S116 stack (TKeyword "≠") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction82 ))
run' S116 stack (TKeyword "{") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction82 ))
run' S116 stack (TKeyword "⟮") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction82 ))
run' S116 stack (TReal arg) = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction82 ))
run' S116 stack (TKeyword "⋆") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction82 ))
run' S116 stack (TKeyword "*") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction82 ))
run' S116 stack (TKeyword "def") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction82 ))
run' S116 stack TEnd = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction82 ))
run' S116 stack (TInteger arg) = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction82 ))
run' S116 stack (TKeyword "∘") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction82 ))
run' S116 stack (TKeyword "⟧") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction82 ))
run' S116 stack (TKeyword "≥") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction82 ))
run' S116 stack (TKeyword "|") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction82 ))
run' S116 stack (TKeyword "=") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction82 ))
run' S116 stack (TKeyword "≡") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction82 ))
run' S116 stack (TKeyword "⟯") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction82 ))
run' S116 stack (TKeyword "→") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction82 ))
run' S116 stack (TKeyword "+") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction82 ))
run' S116 stack (TKeyword "⟨") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction82 ))
run' S116 stack (TKeyword "export") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction82 ))
run' S116 stack (TId arg) = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction82 ))
run' S116 stack (TString arg) = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction82 ))
run' S116 stack (TKeyword "∧") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction82 ))
run' S116 stack (TKeyword ">") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction82 ))
run' S116 stack (TKeyword "}") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction82 ))
run' S116 stack (TKeyword ":") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction82 ))
run' S116 stack (TKeyword "/") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction82 ))
run' S116 stack (TKeyword "⊓") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction82 ))
run' S116 stack (TKeyword "(") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction82 ))
run' S116 stack (TKeyword "import") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction82 ))
run' S116 stack (TKeyword "∨") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction82 ))
run' S116 stack (TKeyword "⟩") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction82 ))
run' S116 stack (TKeyword "]") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction82 ))
run' S116 stack (TKeyword ";") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction82 ))
run' S116 stack (TKeyword "⊔") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction82 ))
run' S116 stack (TKeyword ",") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoPrim s (Sprim stack s (reduction82 ))
run' S117 stack (TReal arg) = shift >> run S44 (Sreal stack S117 arg)
run' S117 stack (TKeyword "⋆") = shift >> run S45 (Skeyword stack S117)
run' S117 stack (TKeyword "(") = shift >> run S96 (Skeyword stack S117)
run' S117 stack (TKeyword "[") = shift >> run S54 (Skeyword stack S117)
run' S117 stack (TInteger arg) = shift >> run S57 (Sinteger stack S117 arg)
run' S117 stack (TId arg) = shift >> run S124 (Sid stack S117 arg)
run' S117 stack (TString arg) = shift >> run S36 (Sstring stack S117 arg)
run' S117 stack (TKeyword "~") = shift >> run S59 (Skeyword stack S117)
run' S117 stack (TKeyword "{") = shift >> run S42 (Skeyword stack S117)
run' S118 stack (TReal arg) = shift >> run S44 (Sreal stack S118 arg)
run' S118 stack (TKeyword "⋆") = shift >> run S45 (Skeyword stack S118)
run' S118 stack (TKeyword "(") = shift >> run S96 (Skeyword stack S118)
run' S118 stack (TKeyword "[") = shift >> run S54 (Skeyword stack S118)
run' S118 stack (TInteger arg) = shift >> run S57 (Sinteger stack S118 arg)
run' S118 stack (TId arg) = shift >> run S124 (Sid stack S118 arg)
run' S118 stack (TString arg) = shift >> run S36 (Sstring stack S118 arg)
run' S118 stack (TKeyword "~") = shift >> run S59 (Skeyword stack S118)
run' S118 stack (TKeyword "{") = shift >> run S42 (Skeyword stack S118)
run' S119 stack (TReal arg) = shift >> run S44 (Sreal stack S119 arg)
run' S119 stack (TKeyword "⋆") = shift >> run S45 (Skeyword stack S119)
run' S119 stack (TKeyword "{") = shift >> run S42 (Skeyword stack S119)
run' S119 stack (TInteger arg) = shift >> run S57 (Sinteger stack S119 arg)
run' S119 stack (TKeyword "~") = shift >> run S59 (Skeyword stack S119)
run' S119 stack (TKeyword "(") = shift >> run S96 (Skeyword stack S119)
run' S119 stack (TKeyword "[") = shift >> run S54 (Skeyword stack S119)
run' S119 stack (TId arg) = shift >> run S124 (Sid stack S119 arg)
run' S119 stack (TString arg) = shift >> run S36 (Sstring stack S119 arg)
run' S119 stack (TKeyword "¬") = shift >> run S63 (Skeyword stack S119)
run' S120 stack (TId arg) = shift >> run S39 (Sid stack S120 arg)
run' S120 stack (TString arg) = shift >> run S36 (Sstring stack S120 arg)
run' S120 stack (TKeyword "{") = shift >> run S42 (Skeyword stack S120)
run' S120 stack (TKeyword "⟮") = shift >> run S43 (Skeyword stack S120)
run' S120 stack (TReal arg) = shift >> run S44 (Sreal stack S120 arg)
run' S120 stack (TKeyword "⋆") = shift >> run S45 (Skeyword stack S120)
run' S120 stack (TKeyword "λ") = shift >> run S46 (Skeyword stack S120)
run' S120 stack (TKeyword "(") = shift >> run S96 (Skeyword stack S120)
run' S120 stack (TKeyword "∃") = shift >> run S52 (Skeyword stack S120)
run' S120 stack (TKeyword "[") = shift >> run S54 (Skeyword stack S120)
run' S120 stack (TInteger arg) = shift >> run S57 (Sinteger stack S120 arg)
run' S120 stack (TKeyword "~") = shift >> run S59 (Skeyword stack S120)
run' S120 stack (TKeyword "¬") = shift >> run S63 (Skeyword stack S120)
run' S121 stack (TKeyword "-") = shift >> run S28 (Skeyword stack S121)
run' S121 stack (TKeyword "⋆") = shift >> run S45 (Skeyword stack S121)
run' S121 stack (TKeyword "⟨") = shift >> run S31 (Skeyword stack S121)
run' S121 stack (TKeyword "λ") = shift >> run S46 (Skeyword stack S121)
run' S121 stack (TKeyword "(") = shift >> run S49 (Skeyword stack S121)
run' S121 stack (TKeyword "∃") = shift >> run S52 (Skeyword stack S121)
run' S121 stack (TKeyword "[") = shift >> run S54 (Skeyword stack S121)
run' S121 stack (TInteger arg) = shift >> run S57 (Sinteger stack S121 arg)
run' S121 stack (TString arg) = shift >> run S36 (Sstring stack S121 arg)
run' S121 stack (TKeyword "⟦") = shift >> run S37 (Skeyword stack S121)
run' S121 stack (TKeyword "~") = shift >> run S59 (Skeyword stack S121)
run' S121 stack (TId arg) = shift >> run S39 (Sid stack S121 arg)
run' S121 stack (TKeyword "{") = shift >> run S42 (Skeyword stack S121)
run' S121 stack (TReal arg) = shift >> run S44 (Sreal stack S121 arg)
run' S121 stack (TKeyword "⟮") = shift >> run S43 (Skeyword stack S121)
run' S121 stack (TKeyword "¬") = shift >> run S63 (Skeyword stack S121)
run' S122 stack (TKeyword "-") = shift >> run S28 (Skeyword stack S122)
run' S122 stack (TKeyword "⋆") = shift >> run S45 (Skeyword stack S122)
run' S122 stack (TKeyword "⟨") = shift >> run S31 (Skeyword stack S122)
run' S122 stack (TKeyword "λ") = shift >> run S46 (Skeyword stack S122)
run' S122 stack (TKeyword "(") = shift >> run S49 (Skeyword stack S122)
run' S122 stack (TKeyword "∃") = shift >> run S52 (Skeyword stack S122)
run' S122 stack (TKeyword "[") = shift >> run S54 (Skeyword stack S122)
run' S122 stack (TInteger arg) = shift >> run S57 (Sinteger stack S122 arg)
run' S122 stack (TString arg) = shift >> run S36 (Sstring stack S122 arg)
run' S122 stack (TKeyword "⟦") = shift >> run S37 (Skeyword stack S122)
run' S122 stack (TKeyword "~") = shift >> run S59 (Skeyword stack S122)
run' S122 stack (TId arg) = shift >> run S39 (Sid stack S122 arg)
run' S122 stack (TKeyword "{") = shift >> run S42 (Skeyword stack S122)
run' S122 stack (TReal arg) = shift >> run S44 (Sreal stack S122 arg)
run' S122 stack (TKeyword "⟮") = shift >> run S43 (Skeyword stack S122)
run' S122 stack (TKeyword "¬") = shift >> run S63 (Skeyword stack S122)
run' S123 stack (TKeyword "-") = case stack of
    (Sprefix (Skeyword stack s) _ x0) ->
        gotoPrefix s (Sprefix stack s (reduction65 x0))
run' S123 stack (TKeyword "+") = case stack of
    (Sprefix (Skeyword stack s) _ x0) ->
        gotoPrefix s (Sprefix stack s (reduction65 x0))
run' S123 stack (TKeyword ")") = case stack of
    (Sprefix (Skeyword stack s) _ x0) ->
        gotoPrefix s (Sprefix stack s (reduction65 x0))
run' S123 stack (TKeyword "export") = case stack of
    (Sprefix (Skeyword stack s) _ x0) ->
        gotoPrefix s (Sprefix stack s (reduction65 x0))
run' S123 stack (TKeyword "import") = case stack of
    (Sprefix (Skeyword stack s) _ x0) ->
        gotoPrefix s (Sprefix stack s (reduction65 x0))
run' S123 stack (TKeyword "∧") = case stack of
    (Sprefix (Skeyword stack s) _ x0) ->
        gotoPrefix s (Sprefix stack s (reduction65 x0))
run' S123 stack (TKeyword "≤") = case stack of
    (Sprefix (Skeyword stack s) _ x0) ->
        gotoPrefix s (Sprefix stack s (reduction65 x0))
run' S123 stack (TKeyword ">") = case stack of
    (Sprefix (Skeyword stack s) _ x0) ->
        gotoPrefix s (Sprefix stack s (reduction65 x0))
run' S123 stack (TKeyword "<") = case stack of
    (Sprefix (Skeyword stack s) _ x0) ->
        gotoPrefix s (Sprefix stack s (reduction65 x0))
run' S123 stack (TKeyword "}") = case stack of
    (Sprefix (Skeyword stack s) _ x0) ->
        gotoPrefix s (Sprefix stack s (reduction65 x0))
run' S123 stack (TKeyword "≠") = case stack of
    (Sprefix (Skeyword stack s) _ x0) ->
        gotoPrefix s (Sprefix stack s (reduction65 x0))
run' S123 stack (TKeyword ":") = case stack of
    (Sprefix (Skeyword stack s) _ x0) ->
        gotoPrefix s (Sprefix stack s (reduction65 x0))
run' S123 stack (TKeyword "/") = case stack of
    (Sprefix (Skeyword stack s) _ x0) ->
        gotoPrefix s (Sprefix stack s (reduction65 x0))
run' S123 stack (TKeyword "⊓") = case stack of
    (Sprefix (Skeyword stack s) _ x0) ->
        gotoPrefix s (Sprefix stack s (reduction65 x0))
run' S123 stack (TKeyword "*") = case stack of
    (Sprefix (Skeyword stack s) _ x0) ->
        gotoPrefix s (Sprefix stack s (reduction65 x0))
run' S123 stack (TKeyword "def") = case stack of
    (Sprefix (Skeyword stack s) _ x0) ->
        gotoPrefix s (Sprefix stack s (reduction65 x0))
run' S123 stack (TKeyword "∨") = case stack of
    (Sprefix (Skeyword stack s) _ x0) ->
        gotoPrefix s (Sprefix stack s (reduction65 x0))
run' S123 stack (TKeyword "⟩") = case stack of
    (Sprefix (Skeyword stack s) _ x0) ->
        gotoPrefix s (Sprefix stack s (reduction65 x0))
run' S123 stack (TKeyword "]") = case stack of
    (Sprefix (Skeyword stack s) _ x0) ->
        gotoPrefix s (Sprefix stack s (reduction65 x0))
run' S123 stack TEnd = case stack of
    (Sprefix (Skeyword stack s) _ x0) ->
        gotoPrefix s (Sprefix stack s (reduction65 x0))
run' S123 stack (TKeyword "∘") = case stack of
    (Sprefix (Skeyword stack s) _ x0) ->
        gotoPrefix s (Sprefix stack s (reduction65 x0))
run' S123 stack (TKeyword "⟧") = case stack of
    (Sprefix (Skeyword stack s) _ x0) ->
        gotoPrefix s (Sprefix stack s (reduction65 x0))
run' S123 stack (TKeyword "≥") = case stack of
    (Sprefix (Skeyword stack s) _ x0) ->
        gotoPrefix s (Sprefix stack s (reduction65 x0))
run' S123 stack (TKeyword "|") = case stack of
    (Sprefix (Skeyword stack s) _ x0) ->
        gotoPrefix s (Sprefix stack s (reduction65 x0))
run' S123 stack (TKeyword "=") = case stack of
    (Sprefix (Skeyword stack s) _ x0) ->
        gotoPrefix s (Sprefix stack s (reduction65 x0))
run' S123 stack (TKeyword "≡") = case stack of
    (Sprefix (Skeyword stack s) _ x0) ->
        gotoPrefix s (Sprefix stack s (reduction65 x0))
run' S123 stack (TKeyword ";") = case stack of
    (Sprefix (Skeyword stack s) _ x0) ->
        gotoPrefix s (Sprefix stack s (reduction65 x0))
run' S123 stack (TKeyword "⊔") = case stack of
    (Sprefix (Skeyword stack s) _ x0) ->
        gotoPrefix s (Sprefix stack s (reduction65 x0))
run' S123 stack (TKeyword "⟯") = case stack of
    (Sprefix (Skeyword stack s) _ x0) ->
        gotoPrefix s (Sprefix stack s (reduction65 x0))
run' S123 stack (TKeyword "→") = case stack of
    (Sprefix (Skeyword stack s) _ x0) ->
        gotoPrefix s (Sprefix stack s (reduction65 x0))
run' S123 stack (TKeyword ",") = case stack of
    (Sprefix (Skeyword stack s) _ x0) ->
        gotoPrefix s (Sprefix stack s (reduction65 x0))
run' S124 stack (TKeyword "-") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S124 stack (TKeyword "[") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S124 stack (TKeyword "^") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S124 stack (TKeyword ")") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S124 stack (TKeyword "⟦") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S124 stack (TKeyword "≤") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S124 stack (TKeyword "<") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S124 stack (TKeyword "≠") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S124 stack (TKeyword "{") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S124 stack (TKeyword "⟮") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S124 stack (TReal arg) = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S124 stack (TKeyword "⋆") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S124 stack (TKeyword "*") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S124 stack (TKeyword "def") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S124 stack TEnd = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S124 stack (TInteger arg) = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S124 stack (TKeyword "∘") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S124 stack (TKeyword "⟧") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S124 stack (TKeyword "≥") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S124 stack (TKeyword "|") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S124 stack (TKeyword "=") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S124 stack (TKeyword "≡") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S124 stack (TKeyword "⟯") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S124 stack (TKeyword "→") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S124 stack (TKeyword "+") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S124 stack (TKeyword "⟨") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S124 stack (TKeyword "export") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S124 stack (TId arg) = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S124 stack (TString arg) = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S124 stack (TKeyword "∧") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S124 stack (TKeyword ">") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S124 stack (TKeyword "}") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S124 stack (TKeyword ":") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S124 stack (TKeyword "/") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S124 stack (TKeyword "⊓") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S124 stack (TKeyword "(") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S124 stack (TKeyword "import") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S124 stack (TKeyword "∨") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S124 stack (TKeyword "⟩") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S124 stack (TKeyword "]") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S124 stack (TKeyword ";") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S124 stack (TKeyword "⊔") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S124 stack (TKeyword ",") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S125 stack (TId arg) = shift >> run S124 (Sid stack S125 arg)
run' S125 stack (TKeyword "{") = shift >> run S42 (Skeyword stack S125)
run' S125 stack (TReal arg) = shift >> run S44 (Sreal stack S125 arg)
run' S125 stack (TKeyword "⋆") = shift >> run S45 (Skeyword stack S125)
run' S125 stack (TString arg) = shift >> run S36 (Sstring stack S125 arg)
run' S125 stack (TKeyword "(") = shift >> run S96 (Skeyword stack S125)
run' S125 stack (TKeyword "[") = shift >> run S54 (Skeyword stack S125)
run' S125 stack (TInteger arg) = shift >> run S57 (Sinteger stack S125 arg)
run' S125 stack (TKeyword "~") = shift >> run S59 (Skeyword stack S125)
run' S125 stack (TKeyword "¬") = shift >> run S63 (Skeyword stack S125)
run' S126 stack (TKeyword "import") = case stack of
    (Snot (Skeyword stack s) _ x0) ->
        gotoNot s (Snot stack s (reduction48 x0))
run' S126 stack (TKeyword "∧") = case stack of
    (Snot (Skeyword stack s) _ x0) ->
        gotoNot s (Snot stack s (reduction48 x0))
run' S126 stack (TKeyword "∨") = case stack of
    (Snot (Skeyword stack s) _ x0) ->
        gotoNot s (Snot stack s (reduction48 x0))
run' S126 stack (TKeyword ";") = case stack of
    (Snot (Skeyword stack s) _ x0) ->
        gotoNot s (Snot stack s (reduction48 x0))
run' S126 stack (TKeyword "⟩") = case stack of
    (Snot (Skeyword stack s) _ x0) ->
        gotoNot s (Snot stack s (reduction48 x0))
run' S126 stack (TKeyword "def") = case stack of
    (Snot (Skeyword stack s) _ x0) ->
        gotoNot s (Snot stack s (reduction48 x0))
run' S126 stack (TKeyword "export") = case stack of
    (Snot (Skeyword stack s) _ x0) ->
        gotoNot s (Snot stack s (reduction48 x0))
run' S126 stack (TKeyword ")") = case stack of
    (Snot (Skeyword stack s) _ x0) ->
        gotoNot s (Snot stack s (reduction48 x0))
run' S126 stack (TKeyword "⟧") = case stack of
    (Snot (Skeyword stack s) _ x0) ->
        gotoNot s (Snot stack s (reduction48 x0))
run' S126 stack (TKeyword "|") = case stack of
    (Snot (Skeyword stack s) _ x0) ->
        gotoNot s (Snot stack s (reduction48 x0))
run' S126 stack (TKeyword "]") = case stack of
    (Snot (Skeyword stack s) _ x0) ->
        gotoNot s (Snot stack s (reduction48 x0))
run' S126 stack (TKeyword "}") = case stack of
    (Snot (Skeyword stack s) _ x0) ->
        gotoNot s (Snot stack s (reduction48 x0))
run' S126 stack (TKeyword "≡") = case stack of
    (Snot (Skeyword stack s) _ x0) ->
        gotoNot s (Snot stack s (reduction48 x0))
run' S126 stack (TKeyword ":") = case stack of
    (Snot (Skeyword stack s) _ x0) ->
        gotoNot s (Snot stack s (reduction48 x0))
run' S126 stack TEnd = case stack of
    (Snot (Skeyword stack s) _ x0) ->
        gotoNot s (Snot stack s (reduction48 x0))
run' S126 stack (TKeyword "⊔") = case stack of
    (Snot (Skeyword stack s) _ x0) ->
        gotoNot s (Snot stack s (reduction48 x0))
run' S126 stack (TKeyword "⟯") = case stack of
    (Snot (Skeyword stack s) _ x0) ->
        gotoNot s (Snot stack s (reduction48 x0))
run' S126 stack (TKeyword "→") = case stack of
    (Snot (Skeyword stack s) _ x0) ->
        gotoNot s (Snot stack s (reduction48 x0))
run' S126 stack (TKeyword "⊓") = case stack of
    (Snot (Skeyword stack s) _ x0) ->
        gotoNot s (Snot stack s (reduction48 x0))
run' S126 stack (TKeyword ",") = case stack of
    (Snot (Skeyword stack s) _ x0) ->
        gotoNot s (Snot stack s (reduction48 x0))
run' S127 stack (TKeyword "⟩") = case stack of
    (Slabbinds (Skeyword (Slabbind stack s x0) _) _ x1) ->
        gotoLabbinds s (Slabbinds stack s (reduction101 x0 x1))
run' S128 stack (TReal arg) = case stack of
    (Skeyword (Slabtypes (Skeyword stack s) _ x0) _) ->
        gotoStructdef s (Sstructdef stack s (reduction90 x0))
run' S128 stack (TKeyword "⋆") = case stack of
    (Skeyword (Slabtypes (Skeyword stack s) _ x0) _) ->
        gotoStructdef s (Sstructdef stack s (reduction90 x0))
run' S128 stack (TKeyword "λ") = case stack of
    (Skeyword (Slabtypes (Skeyword stack s) _ x0) _) ->
        gotoStructdef s (Sstructdef stack s (reduction90 x0))
run' S128 stack (TKeyword "(") = case stack of
    (Skeyword (Slabtypes (Skeyword stack s) _ x0) _) ->
        gotoStructdef s (Sstructdef stack s (reduction90 x0))
run' S128 stack (TKeyword "∃") = case stack of
    (Skeyword (Slabtypes (Skeyword stack s) _ x0) _) ->
        gotoStructdef s (Sstructdef stack s (reduction90 x0))
run' S128 stack (TKeyword "[") = case stack of
    (Skeyword (Slabtypes (Skeyword stack s) _ x0) _) ->
        gotoStructdef s (Sstructdef stack s (reduction90 x0))
run' S128 stack (TInteger arg) = case stack of
    (Skeyword (Slabtypes (Skeyword stack s) _ x0) _) ->
        gotoStructdef s (Sstructdef stack s (reduction90 x0))
run' S128 stack (TId arg) = case stack of
    (Skeyword (Slabtypes (Skeyword stack s) _ x0) _) ->
        gotoStructdef s (Sstructdef stack s (reduction90 x0))
run' S128 stack (TString arg) = case stack of
    (Skeyword (Slabtypes (Skeyword stack s) _ x0) _) ->
        gotoStructdef s (Sstructdef stack s (reduction90 x0))
run' S128 stack (TKeyword "~") = case stack of
    (Skeyword (Slabtypes (Skeyword stack s) _ x0) _) ->
        gotoStructdef s (Sstructdef stack s (reduction90 x0))
run' S128 stack (TKeyword "{") = case stack of
    (Skeyword (Slabtypes (Skeyword stack s) _ x0) _) ->
        gotoStructdef s (Sstructdef stack s (reduction90 x0))
run' S128 stack (TKeyword "⟮") = case stack of
    (Skeyword (Slabtypes (Skeyword stack s) _ x0) _) ->
        gotoStructdef s (Sstructdef stack s (reduction90 x0))
run' S128 stack (TKeyword "¬") = case stack of
    (Skeyword (Slabtypes (Skeyword stack s) _ x0) _) ->
        gotoStructdef s (Sstructdef stack s (reduction90 x0))
run' S129 stack (TId arg) = shift >> run S75 (Sid stack S129 arg)
run' S130 stack (TKeyword "-") = shift >> run S28 (Skeyword stack S130)
run' S130 stack (TKeyword "⋆") = shift >> run S45 (Skeyword stack S130)
run' S130 stack (TKeyword "⟨") = shift >> run S31 (Skeyword stack S130)
run' S130 stack (TKeyword "λ") = shift >> run S46 (Skeyword stack S130)
run' S130 stack (TKeyword "(") = shift >> run S49 (Skeyword stack S130)
run' S130 stack (TKeyword "∃") = shift >> run S52 (Skeyword stack S130)
run' S130 stack (TKeyword "[") = shift >> run S54 (Skeyword stack S130)
run' S130 stack (TInteger arg) = shift >> run S57 (Sinteger stack S130 arg)
run' S130 stack (TString arg) = shift >> run S36 (Sstring stack S130 arg)
run' S130 stack (TKeyword "⟦") = shift >> run S37 (Skeyword stack S130)
run' S130 stack (TKeyword "~") = shift >> run S59 (Skeyword stack S130)
run' S130 stack (TId arg) = shift >> run S39 (Sid stack S130 arg)
run' S130 stack (TKeyword "{") = shift >> run S42 (Skeyword stack S130)
run' S130 stack (TReal arg) = shift >> run S44 (Sreal stack S130 arg)
run' S130 stack (TKeyword "⟮") = shift >> run S43 (Skeyword stack S130)
run' S130 stack (TKeyword "¬") = shift >> run S63 (Skeyword stack S130)
run' S131 stack (TKeyword "import") = case stack of
    (Sconj2 (Skeyword (Sdisj2 stack s x0) _) _ x1) ->
        gotoConj2 s (Sconj2 stack s (reduction42 x0 x1))
run' S131 stack (TKeyword "⟧") = case stack of
    (Sconj2 (Skeyword (Sdisj2 stack s x0) _) _ x1) ->
        gotoConj2 s (Sconj2 stack s (reduction42 x0 x1))
run' S131 stack (TKeyword ";") = case stack of
    (Sconj2 (Skeyword (Sdisj2 stack s x0) _) _ x1) ->
        gotoConj2 s (Sconj2 stack s (reduction42 x0 x1))
run' S131 stack (TKeyword "⟩") = case stack of
    (Sconj2 (Skeyword (Sdisj2 stack s x0) _) _ x1) ->
        gotoConj2 s (Sconj2 stack s (reduction42 x0 x1))
run' S131 stack (TKeyword "def") = case stack of
    (Sconj2 (Skeyword (Sdisj2 stack s x0) _) _ x1) ->
        gotoConj2 s (Sconj2 stack s (reduction42 x0 x1))
run' S131 stack (TKeyword "export") = case stack of
    (Sconj2 (Skeyword (Sdisj2 stack s x0) _) _ x1) ->
        gotoConj2 s (Sconj2 stack s (reduction42 x0 x1))
run' S131 stack (TKeyword ")") = case stack of
    (Sconj2 (Skeyword (Sdisj2 stack s x0) _) _ x1) ->
        gotoConj2 s (Sconj2 stack s (reduction42 x0 x1))
run' S131 stack (TKeyword "|") = case stack of
    (Sconj2 (Skeyword (Sdisj2 stack s x0) _) _ x1) ->
        gotoConj2 s (Sconj2 stack s (reduction42 x0 x1))
run' S131 stack (TKeyword "]") = case stack of
    (Sconj2 (Skeyword (Sdisj2 stack s x0) _) _ x1) ->
        gotoConj2 s (Sconj2 stack s (reduction42 x0 x1))
run' S131 stack (TKeyword "}") = case stack of
    (Sconj2 (Skeyword (Sdisj2 stack s x0) _) _ x1) ->
        gotoConj2 s (Sconj2 stack s (reduction42 x0 x1))
run' S131 stack (TKeyword "≡") = case stack of
    (Sconj2 (Skeyword (Sdisj2 stack s x0) _) _ x1) ->
        gotoConj2 s (Sconj2 stack s (reduction42 x0 x1))
run' S131 stack (TKeyword ":") = case stack of
    (Sconj2 (Skeyword (Sdisj2 stack s x0) _) _ x1) ->
        gotoConj2 s (Sconj2 stack s (reduction42 x0 x1))
run' S131 stack TEnd = case stack of
    (Sconj2 (Skeyword (Sdisj2 stack s x0) _) _ x1) ->
        gotoConj2 s (Sconj2 stack s (reduction42 x0 x1))
run' S131 stack (TKeyword "⊔") = case stack of
    (Sconj2 (Skeyword (Sdisj2 stack s x0) _) _ x1) ->
        gotoConj2 s (Sconj2 stack s (reduction42 x0 x1))
run' S131 stack (TKeyword "⟯") = case stack of
    (Sconj2 (Skeyword (Sdisj2 stack s x0) _) _ x1) ->
        gotoConj2 s (Sconj2 stack s (reduction42 x0 x1))
run' S131 stack (TKeyword "→") = case stack of
    (Sconj2 (Skeyword (Sdisj2 stack s x0) _) _ x1) ->
        gotoConj2 s (Sconj2 stack s (reduction42 x0 x1))
run' S131 stack (TKeyword ",") = case stack of
    (Sconj2 (Skeyword (Sdisj2 stack s x0) _) _ x1) ->
        gotoConj2 s (Sconj2 stack s (reduction42 x0 x1))
run' S132 stack (TKeyword "-") = case stack of
    (Smul (Skeyword (Sprefix stack s x0) _) _ x1) ->
        gotoMul s (Smul stack s (reduction63 x0 x1))
run' S132 stack (TKeyword "+") = case stack of
    (Smul (Skeyword (Sprefix stack s x0) _) _ x1) ->
        gotoMul s (Smul stack s (reduction63 x0 x1))
run' S132 stack (TKeyword ")") = case stack of
    (Smul (Skeyword (Sprefix stack s x0) _) _ x1) ->
        gotoMul s (Smul stack s (reduction63 x0 x1))
run' S132 stack (TKeyword "export") = case stack of
    (Smul (Skeyword (Sprefix stack s x0) _) _ x1) ->
        gotoMul s (Smul stack s (reduction63 x0 x1))
run' S132 stack (TKeyword "import") = case stack of
    (Smul (Skeyword (Sprefix stack s x0) _) _ x1) ->
        gotoMul s (Smul stack s (reduction63 x0 x1))
run' S132 stack (TKeyword "∧") = case stack of
    (Smul (Skeyword (Sprefix stack s x0) _) _ x1) ->
        gotoMul s (Smul stack s (reduction63 x0 x1))
run' S132 stack (TKeyword "≤") = case stack of
    (Smul (Skeyword (Sprefix stack s x0) _) _ x1) ->
        gotoMul s (Smul stack s (reduction63 x0 x1))
run' S132 stack (TKeyword ">") = case stack of
    (Smul (Skeyword (Sprefix stack s x0) _) _ x1) ->
        gotoMul s (Smul stack s (reduction63 x0 x1))
run' S132 stack (TKeyword "<") = case stack of
    (Smul (Skeyword (Sprefix stack s x0) _) _ x1) ->
        gotoMul s (Smul stack s (reduction63 x0 x1))
run' S132 stack (TKeyword "}") = case stack of
    (Smul (Skeyword (Sprefix stack s x0) _) _ x1) ->
        gotoMul s (Smul stack s (reduction63 x0 x1))
run' S132 stack (TKeyword "≠") = case stack of
    (Smul (Skeyword (Sprefix stack s x0) _) _ x1) ->
        gotoMul s (Smul stack s (reduction63 x0 x1))
run' S132 stack (TKeyword ":") = case stack of
    (Smul (Skeyword (Sprefix stack s x0) _) _ x1) ->
        gotoMul s (Smul stack s (reduction63 x0 x1))
run' S132 stack (TKeyword "⊓") = case stack of
    (Smul (Skeyword (Sprefix stack s x0) _) _ x1) ->
        gotoMul s (Smul stack s (reduction63 x0 x1))
run' S132 stack (TKeyword "def") = case stack of
    (Smul (Skeyword (Sprefix stack s x0) _) _ x1) ->
        gotoMul s (Smul stack s (reduction63 x0 x1))
run' S132 stack (TKeyword "∨") = case stack of
    (Smul (Skeyword (Sprefix stack s x0) _) _ x1) ->
        gotoMul s (Smul stack s (reduction63 x0 x1))
run' S132 stack (TKeyword "⟩") = case stack of
    (Smul (Skeyword (Sprefix stack s x0) _) _ x1) ->
        gotoMul s (Smul stack s (reduction63 x0 x1))
run' S132 stack (TKeyword "]") = case stack of
    (Smul (Skeyword (Sprefix stack s x0) _) _ x1) ->
        gotoMul s (Smul stack s (reduction63 x0 x1))
run' S132 stack TEnd = case stack of
    (Smul (Skeyword (Sprefix stack s x0) _) _ x1) ->
        gotoMul s (Smul stack s (reduction63 x0 x1))
run' S132 stack (TKeyword "∘") = case stack of
    (Smul (Skeyword (Sprefix stack s x0) _) _ x1) ->
        gotoMul s (Smul stack s (reduction63 x0 x1))
run' S132 stack (TKeyword "⟧") = case stack of
    (Smul (Skeyword (Sprefix stack s x0) _) _ x1) ->
        gotoMul s (Smul stack s (reduction63 x0 x1))
run' S132 stack (TKeyword "≥") = case stack of
    (Smul (Skeyword (Sprefix stack s x0) _) _ x1) ->
        gotoMul s (Smul stack s (reduction63 x0 x1))
run' S132 stack (TKeyword "|") = case stack of
    (Smul (Skeyword (Sprefix stack s x0) _) _ x1) ->
        gotoMul s (Smul stack s (reduction63 x0 x1))
run' S132 stack (TKeyword "=") = case stack of
    (Smul (Skeyword (Sprefix stack s x0) _) _ x1) ->
        gotoMul s (Smul stack s (reduction63 x0 x1))
run' S132 stack (TKeyword "≡") = case stack of
    (Smul (Skeyword (Sprefix stack s x0) _) _ x1) ->
        gotoMul s (Smul stack s (reduction63 x0 x1))
run' S132 stack (TKeyword ";") = case stack of
    (Smul (Skeyword (Sprefix stack s x0) _) _ x1) ->
        gotoMul s (Smul stack s (reduction63 x0 x1))
run' S132 stack (TKeyword "⊔") = case stack of
    (Smul (Skeyword (Sprefix stack s x0) _) _ x1) ->
        gotoMul s (Smul stack s (reduction63 x0 x1))
run' S132 stack (TKeyword "⟯") = case stack of
    (Smul (Skeyword (Sprefix stack s x0) _) _ x1) ->
        gotoMul s (Smul stack s (reduction63 x0 x1))
run' S132 stack (TKeyword "→") = case stack of
    (Smul (Skeyword (Sprefix stack s x0) _) _ x1) ->
        gotoMul s (Smul stack s (reduction63 x0 x1))
run' S132 stack (TKeyword ",") = case stack of
    (Smul (Skeyword (Sprefix stack s x0) _) _ x1) ->
        gotoMul s (Smul stack s (reduction63 x0 x1))
run' S133 stack (TKeyword "-") = case stack of
    (Smul (Skeyword (Sprefix stack s x0) _) _ x1) ->
        gotoMul s (Smul stack s (reduction62 x0 x1))
run' S133 stack (TKeyword "+") = case stack of
    (Smul (Skeyword (Sprefix stack s x0) _) _ x1) ->
        gotoMul s (Smul stack s (reduction62 x0 x1))
run' S133 stack (TKeyword ")") = case stack of
    (Smul (Skeyword (Sprefix stack s x0) _) _ x1) ->
        gotoMul s (Smul stack s (reduction62 x0 x1))
run' S133 stack (TKeyword "export") = case stack of
    (Smul (Skeyword (Sprefix stack s x0) _) _ x1) ->
        gotoMul s (Smul stack s (reduction62 x0 x1))
run' S133 stack (TKeyword "import") = case stack of
    (Smul (Skeyword (Sprefix stack s x0) _) _ x1) ->
        gotoMul s (Smul stack s (reduction62 x0 x1))
run' S133 stack (TKeyword "∧") = case stack of
    (Smul (Skeyword (Sprefix stack s x0) _) _ x1) ->
        gotoMul s (Smul stack s (reduction62 x0 x1))
run' S133 stack (TKeyword "≤") = case stack of
    (Smul (Skeyword (Sprefix stack s x0) _) _ x1) ->
        gotoMul s (Smul stack s (reduction62 x0 x1))
run' S133 stack (TKeyword ">") = case stack of
    (Smul (Skeyword (Sprefix stack s x0) _) _ x1) ->
        gotoMul s (Smul stack s (reduction62 x0 x1))
run' S133 stack (TKeyword "<") = case stack of
    (Smul (Skeyword (Sprefix stack s x0) _) _ x1) ->
        gotoMul s (Smul stack s (reduction62 x0 x1))
run' S133 stack (TKeyword "}") = case stack of
    (Smul (Skeyword (Sprefix stack s x0) _) _ x1) ->
        gotoMul s (Smul stack s (reduction62 x0 x1))
run' S133 stack (TKeyword "≠") = case stack of
    (Smul (Skeyword (Sprefix stack s x0) _) _ x1) ->
        gotoMul s (Smul stack s (reduction62 x0 x1))
run' S133 stack (TKeyword ":") = case stack of
    (Smul (Skeyword (Sprefix stack s x0) _) _ x1) ->
        gotoMul s (Smul stack s (reduction62 x0 x1))
run' S133 stack (TKeyword "⊓") = case stack of
    (Smul (Skeyword (Sprefix stack s x0) _) _ x1) ->
        gotoMul s (Smul stack s (reduction62 x0 x1))
run' S133 stack (TKeyword "def") = case stack of
    (Smul (Skeyword (Sprefix stack s x0) _) _ x1) ->
        gotoMul s (Smul stack s (reduction62 x0 x1))
run' S133 stack (TKeyword "∨") = case stack of
    (Smul (Skeyword (Sprefix stack s x0) _) _ x1) ->
        gotoMul s (Smul stack s (reduction62 x0 x1))
run' S133 stack (TKeyword "⟩") = case stack of
    (Smul (Skeyword (Sprefix stack s x0) _) _ x1) ->
        gotoMul s (Smul stack s (reduction62 x0 x1))
run' S133 stack (TKeyword "]") = case stack of
    (Smul (Skeyword (Sprefix stack s x0) _) _ x1) ->
        gotoMul s (Smul stack s (reduction62 x0 x1))
run' S133 stack TEnd = case stack of
    (Smul (Skeyword (Sprefix stack s x0) _) _ x1) ->
        gotoMul s (Smul stack s (reduction62 x0 x1))
run' S133 stack (TKeyword "∘") = case stack of
    (Smul (Skeyword (Sprefix stack s x0) _) _ x1) ->
        gotoMul s (Smul stack s (reduction62 x0 x1))
run' S133 stack (TKeyword "⟧") = case stack of
    (Smul (Skeyword (Sprefix stack s x0) _) _ x1) ->
        gotoMul s (Smul stack s (reduction62 x0 x1))
run' S133 stack (TKeyword "≥") = case stack of
    (Smul (Skeyword (Sprefix stack s x0) _) _ x1) ->
        gotoMul s (Smul stack s (reduction62 x0 x1))
run' S133 stack (TKeyword "|") = case stack of
    (Smul (Skeyword (Sprefix stack s x0) _) _ x1) ->
        gotoMul s (Smul stack s (reduction62 x0 x1))
run' S133 stack (TKeyword "=") = case stack of
    (Smul (Skeyword (Sprefix stack s x0) _) _ x1) ->
        gotoMul s (Smul stack s (reduction62 x0 x1))
run' S133 stack (TKeyword "≡") = case stack of
    (Smul (Skeyword (Sprefix stack s x0) _) _ x1) ->
        gotoMul s (Smul stack s (reduction62 x0 x1))
run' S133 stack (TKeyword ";") = case stack of
    (Smul (Skeyword (Sprefix stack s x0) _) _ x1) ->
        gotoMul s (Smul stack s (reduction62 x0 x1))
run' S133 stack (TKeyword "⊔") = case stack of
    (Smul (Skeyword (Sprefix stack s x0) _) _ x1) ->
        gotoMul s (Smul stack s (reduction62 x0 x1))
run' S133 stack (TKeyword "⟯") = case stack of
    (Smul (Skeyword (Sprefix stack s x0) _) _ x1) ->
        gotoMul s (Smul stack s (reduction62 x0 x1))
run' S133 stack (TKeyword "→") = case stack of
    (Smul (Skeyword (Sprefix stack s x0) _) _ x1) ->
        gotoMul s (Smul stack s (reduction62 x0 x1))
run' S133 stack (TKeyword ",") = case stack of
    (Smul (Skeyword (Sprefix stack s x0) _) _ x1) ->
        gotoMul s (Smul stack s (reduction62 x0 x1))
run' S134 stack (TKeyword "import") = case stack of
    (Sdisj2 (Skeyword (Sconj stack s x0) _) _ x1) ->
        gotoDisj2 s (Sdisj2 stack s (reduction44 x0 x1))
run' S134 stack (TKeyword "⟧") = case stack of
    (Sdisj2 (Skeyword (Sconj stack s x0) _) _ x1) ->
        gotoDisj2 s (Sdisj2 stack s (reduction44 x0 x1))
run' S134 stack (TKeyword ";") = case stack of
    (Sdisj2 (Skeyword (Sconj stack s x0) _) _ x1) ->
        gotoDisj2 s (Sdisj2 stack s (reduction44 x0 x1))
run' S134 stack (TKeyword "⟩") = case stack of
    (Sdisj2 (Skeyword (Sconj stack s x0) _) _ x1) ->
        gotoDisj2 s (Sdisj2 stack s (reduction44 x0 x1))
run' S134 stack (TKeyword "def") = case stack of
    (Sdisj2 (Skeyword (Sconj stack s x0) _) _ x1) ->
        gotoDisj2 s (Sdisj2 stack s (reduction44 x0 x1))
run' S134 stack (TKeyword "export") = case stack of
    (Sdisj2 (Skeyword (Sconj stack s x0) _) _ x1) ->
        gotoDisj2 s (Sdisj2 stack s (reduction44 x0 x1))
run' S134 stack (TKeyword ")") = case stack of
    (Sdisj2 (Skeyword (Sconj stack s x0) _) _ x1) ->
        gotoDisj2 s (Sdisj2 stack s (reduction44 x0 x1))
run' S134 stack (TKeyword "|") = case stack of
    (Sdisj2 (Skeyword (Sconj stack s x0) _) _ x1) ->
        gotoDisj2 s (Sdisj2 stack s (reduction44 x0 x1))
run' S134 stack (TKeyword "]") = case stack of
    (Sdisj2 (Skeyword (Sconj stack s x0) _) _ x1) ->
        gotoDisj2 s (Sdisj2 stack s (reduction44 x0 x1))
run' S134 stack (TKeyword "}") = case stack of
    (Sdisj2 (Skeyword (Sconj stack s x0) _) _ x1) ->
        gotoDisj2 s (Sdisj2 stack s (reduction44 x0 x1))
run' S134 stack (TKeyword "≡") = case stack of
    (Sdisj2 (Skeyword (Sconj stack s x0) _) _ x1) ->
        gotoDisj2 s (Sdisj2 stack s (reduction44 x0 x1))
run' S134 stack (TKeyword ":") = case stack of
    (Sdisj2 (Skeyword (Sconj stack s x0) _) _ x1) ->
        gotoDisj2 s (Sdisj2 stack s (reduction44 x0 x1))
run' S134 stack TEnd = case stack of
    (Sdisj2 (Skeyword (Sconj stack s x0) _) _ x1) ->
        gotoDisj2 s (Sdisj2 stack s (reduction44 x0 x1))
run' S134 stack (TKeyword "⊔") = case stack of
    (Sdisj2 (Skeyword (Sconj stack s x0) _) _ x1) ->
        gotoDisj2 s (Sdisj2 stack s (reduction44 x0 x1))
run' S134 stack (TKeyword "⟯") = case stack of
    (Sdisj2 (Skeyword (Sconj stack s x0) _) _ x1) ->
        gotoDisj2 s (Sdisj2 stack s (reduction44 x0 x1))
run' S134 stack (TKeyword "→") = case stack of
    (Sdisj2 (Skeyword (Sconj stack s x0) _) _ x1) ->
        gotoDisj2 s (Sdisj2 stack s (reduction44 x0 x1))
run' S134 stack (TKeyword "⊓") = case stack of
    (Sdisj2 (Skeyword (Sconj stack s x0) _) _ x1) ->
        gotoDisj2 s (Sdisj2 stack s (reduction44 x0 x1))
run' S134 stack (TKeyword ",") = case stack of
    (Sdisj2 (Skeyword (Sconj stack s x0) _) _ x1) ->
        gotoDisj2 s (Sdisj2 stack s (reduction44 x0 x1))
run' S135 stack (TId arg) = shift >> run S39 (Sid stack S135 arg)
run' S135 stack (TString arg) = shift >> run S36 (Sstring stack S135 arg)
run' S135 stack (TKeyword "{") = shift >> run S42 (Skeyword stack S135)
run' S135 stack (TKeyword "⟮") = shift >> run S43 (Skeyword stack S135)
run' S135 stack (TReal arg) = shift >> run S44 (Sreal stack S135 arg)
run' S135 stack (TKeyword "⋆") = shift >> run S45 (Skeyword stack S135)
run' S135 stack (TKeyword "λ") = shift >> run S46 (Skeyword stack S135)
run' S135 stack (TKeyword "(") = shift >> run S96 (Skeyword stack S135)
run' S135 stack (TKeyword "∃") = shift >> run S52 (Skeyword stack S135)
run' S135 stack (TKeyword "[") = shift >> run S54 (Skeyword stack S135)
run' S135 stack (TInteger arg) = shift >> run S57 (Sinteger stack S135 arg)
run' S135 stack (TKeyword "~") = shift >> run S59 (Skeyword stack S135)
run' S135 stack (TKeyword "¬") = shift >> run S63 (Skeyword stack S135)
run' S136 stack (TReal arg) = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S136 stack (TKeyword "↦") = shift >> run S83 (Skeyword stack S136)
run' S136 stack (TKeyword "←") = shift >> run S84 (Skeyword stack S136)
run' S136 stack (TKeyword "{") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S136 stack (TKeyword "⟨") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S136 stack (TKeyword "⋆") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S136 stack (TKeyword "^") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S136 stack (TKeyword "(") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S136 stack (TKeyword ":") = shift >> run S185 (Skeyword stack S136)
run' S136 stack (TKeyword "[") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S136 stack (TInteger arg) = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S136 stack (TKeyword "⟮") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S136 stack (TKeyword "⟦") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S136 stack (TKeyword "is") = shift >> run S85 (Skeyword stack S136)
run' S136 stack (TId arg) = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S136 stack (TString arg) = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S137 stack (TKeyword "-") = shift >> run S28 (Skeyword stack S137)
run' S137 stack (TKeyword "⋆") = shift >> run S45 (Skeyword stack S137)
run' S137 stack (TKeyword "⟨") = shift >> run S31 (Skeyword stack S137)
run' S137 stack (TKeyword "λ") = shift >> run S46 (Skeyword stack S137)
run' S137 stack (TKeyword "(") = shift >> run S49 (Skeyword stack S137)
run' S137 stack (TKeyword "∃") = shift >> run S52 (Skeyword stack S137)
run' S137 stack (TKeyword "[") = shift >> run S54 (Skeyword stack S137)
run' S137 stack (TInteger arg) = shift >> run S57 (Sinteger stack S137 arg)
run' S137 stack (TString arg) = shift >> run S36 (Sstring stack S137 arg)
run' S137 stack (TKeyword "⟦") = shift >> run S37 (Skeyword stack S137)
run' S137 stack (TKeyword "~") = shift >> run S59 (Skeyword stack S137)
run' S137 stack (TId arg) = shift >> run S39 (Sid stack S137 arg)
run' S137 stack (TKeyword "{") = shift >> run S42 (Skeyword stack S137)
run' S137 stack (TReal arg) = shift >> run S44 (Sreal stack S137 arg)
run' S137 stack (TKeyword "⟮") = shift >> run S43 (Skeyword stack S137)
run' S137 stack (TKeyword "¬") = shift >> run S63 (Skeyword stack S137)
run' S138 stack (TKeyword "-") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S138 stack (TKeyword "[") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S138 stack (TKeyword "^") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S138 stack (TKeyword ")") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S138 stack (TKeyword "⟦") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S138 stack (TKeyword "≤") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S138 stack (TKeyword "<") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S138 stack (TKeyword "≠") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S138 stack (TKeyword "{") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S138 stack (TKeyword "⟮") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S138 stack (TReal arg) = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S138 stack (TKeyword "⋆") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S138 stack (TKeyword "*") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S138 stack (TKeyword "def") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S138 stack TEnd = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S138 stack (TInteger arg) = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S138 stack (TKeyword "∘") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S138 stack (TKeyword "⟧") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S138 stack (TKeyword "≥") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S138 stack (TKeyword "|") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S138 stack (TKeyword "=") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S138 stack (TKeyword "≡") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S138 stack (TKeyword "⟯") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S138 stack (TKeyword "→") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S138 stack (TKeyword "+") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S138 stack (TKeyword "⟨") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S138 stack (TKeyword "export") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S138 stack (TId arg) = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S138 stack (TString arg) = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S138 stack (TKeyword "∧") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S138 stack (TKeyword ">") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S138 stack (TKeyword "}") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S138 stack (TKeyword ":") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S138 stack (TKeyword "/") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S138 stack (TKeyword "⊓") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S138 stack (TKeyword "(") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S138 stack (TKeyword "import") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S138 stack (TKeyword "∨") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S138 stack (TKeyword "⟩") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S138 stack (TKeyword "]") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S138 stack (TKeyword "↦") = shift >> run S83 (Skeyword stack S138)
run' S138 stack (TKeyword ";") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S138 stack (TKeyword "⊔") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S138 stack (TKeyword ",") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S139 stack (TKeyword "import") = case stack of
    (Scocase (Skeyword (Sid stack s x0) _) _ x1) ->
        gotoCocase s (Scocase stack s (reduction37 x0 x1))
run' S139 stack (TKeyword "⟧") = case stack of
    (Scocase (Skeyword (Sid stack s x0) _) _ x1) ->
        gotoCocase s (Scocase stack s (reduction37 x0 x1))
run' S139 stack (TKeyword ";") = case stack of
    (Scocase (Skeyword (Sid stack s x0) _) _ x1) ->
        gotoCocase s (Scocase stack s (reduction37 x0 x1))
run' S139 stack (TKeyword "⟩") = case stack of
    (Scocase (Skeyword (Sid stack s x0) _) _ x1) ->
        gotoCocase s (Scocase stack s (reduction37 x0 x1))
run' S139 stack (TKeyword "def") = case stack of
    (Scocase (Skeyword (Sid stack s x0) _) _ x1) ->
        gotoCocase s (Scocase stack s (reduction37 x0 x1))
run' S139 stack (TKeyword "export") = case stack of
    (Scocase (Skeyword (Sid stack s x0) _) _ x1) ->
        gotoCocase s (Scocase stack s (reduction37 x0 x1))
run' S139 stack (TKeyword ")") = case stack of
    (Scocase (Skeyword (Sid stack s x0) _) _ x1) ->
        gotoCocase s (Scocase stack s (reduction37 x0 x1))
run' S139 stack (TKeyword "|") = case stack of
    (Scocase (Skeyword (Sid stack s x0) _) _ x1) ->
        gotoCocase s (Scocase stack s (reduction37 x0 x1))
run' S139 stack (TKeyword "]") = case stack of
    (Scocase (Skeyword (Sid stack s x0) _) _ x1) ->
        gotoCocase s (Scocase stack s (reduction37 x0 x1))
run' S139 stack (TKeyword "}") = case stack of
    (Scocase (Skeyword (Sid stack s x0) _) _ x1) ->
        gotoCocase s (Scocase stack s (reduction37 x0 x1))
run' S139 stack (TKeyword "≡") = case stack of
    (Scocase (Skeyword (Sid stack s x0) _) _ x1) ->
        gotoCocase s (Scocase stack s (reduction37 x0 x1))
run' S139 stack (TKeyword ":") = case stack of
    (Scocase (Skeyword (Sid stack s x0) _) _ x1) ->
        gotoCocase s (Scocase stack s (reduction37 x0 x1))
run' S139 stack TEnd = case stack of
    (Scocase (Skeyword (Sid stack s x0) _) _ x1) ->
        gotoCocase s (Scocase stack s (reduction37 x0 x1))
run' S139 stack (TKeyword "⟯") = case stack of
    (Scocase (Skeyword (Sid stack s x0) _) _ x1) ->
        gotoCocase s (Scocase stack s (reduction37 x0 x1))
run' S139 stack (TKeyword "→") = case stack of
    (Scocase (Skeyword (Sid stack s x0) _) _ x1) ->
        gotoCocase s (Scocase stack s (reduction37 x0 x1))
run' S139 stack (TKeyword ",") = case stack of
    (Scocase (Skeyword (Sid stack s x0) _) _ x1) ->
        gotoCocase s (Scocase stack s (reduction37 x0 x1))
run' S140 stack (TKeyword ";") = shift >> run S187 (Skeyword stack S140)
run' S141 stack (TKeyword ";") = shift >> run S188 (Skeyword stack S141)
run' S142 stack (TKeyword ")") = case stack of
    (Scomp (Skeyword (Sadd stack s x0) _) _ x1) ->
        gotoComp s (Scomp stack s (reduction57 x0 x1))
run' S142 stack (TKeyword "export") = case stack of
    (Scomp (Skeyword (Sadd stack s x0) _) _ x1) ->
        gotoComp s (Scomp stack s (reduction57 x0 x1))
run' S142 stack (TKeyword "import") = case stack of
    (Scomp (Skeyword (Sadd stack s x0) _) _ x1) ->
        gotoComp s (Scomp stack s (reduction57 x0 x1))
run' S142 stack (TKeyword "∧") = case stack of
    (Scomp (Skeyword (Sadd stack s x0) _) _ x1) ->
        gotoComp s (Scomp stack s (reduction57 x0 x1))
run' S142 stack (TKeyword "≤") = case stack of
    (Scomp (Skeyword (Sadd stack s x0) _) _ x1) ->
        gotoComp s (Scomp stack s (reduction57 x0 x1))
run' S142 stack (TKeyword ">") = case stack of
    (Scomp (Skeyword (Sadd stack s x0) _) _ x1) ->
        gotoComp s (Scomp stack s (reduction57 x0 x1))
run' S142 stack (TKeyword "<") = case stack of
    (Scomp (Skeyword (Sadd stack s x0) _) _ x1) ->
        gotoComp s (Scomp stack s (reduction57 x0 x1))
run' S142 stack (TKeyword "}") = case stack of
    (Scomp (Skeyword (Sadd stack s x0) _) _ x1) ->
        gotoComp s (Scomp stack s (reduction57 x0 x1))
run' S142 stack (TKeyword "≠") = case stack of
    (Scomp (Skeyword (Sadd stack s x0) _) _ x1) ->
        gotoComp s (Scomp stack s (reduction57 x0 x1))
run' S142 stack (TKeyword ":") = case stack of
    (Scomp (Skeyword (Sadd stack s x0) _) _ x1) ->
        gotoComp s (Scomp stack s (reduction57 x0 x1))
run' S142 stack (TKeyword "⊓") = case stack of
    (Scomp (Skeyword (Sadd stack s x0) _) _ x1) ->
        gotoComp s (Scomp stack s (reduction57 x0 x1))
run' S142 stack (TKeyword "def") = case stack of
    (Scomp (Skeyword (Sadd stack s x0) _) _ x1) ->
        gotoComp s (Scomp stack s (reduction57 x0 x1))
run' S142 stack (TKeyword "∨") = case stack of
    (Scomp (Skeyword (Sadd stack s x0) _) _ x1) ->
        gotoComp s (Scomp stack s (reduction57 x0 x1))
run' S142 stack (TKeyword "⟩") = case stack of
    (Scomp (Skeyword (Sadd stack s x0) _) _ x1) ->
        gotoComp s (Scomp stack s (reduction57 x0 x1))
run' S142 stack (TKeyword "]") = case stack of
    (Scomp (Skeyword (Sadd stack s x0) _) _ x1) ->
        gotoComp s (Scomp stack s (reduction57 x0 x1))
run' S142 stack TEnd = case stack of
    (Scomp (Skeyword (Sadd stack s x0) _) _ x1) ->
        gotoComp s (Scomp stack s (reduction57 x0 x1))
run' S142 stack (TKeyword "⟧") = case stack of
    (Scomp (Skeyword (Sadd stack s x0) _) _ x1) ->
        gotoComp s (Scomp stack s (reduction57 x0 x1))
run' S142 stack (TKeyword "≥") = case stack of
    (Scomp (Skeyword (Sadd stack s x0) _) _ x1) ->
        gotoComp s (Scomp stack s (reduction57 x0 x1))
run' S142 stack (TKeyword "|") = case stack of
    (Scomp (Skeyword (Sadd stack s x0) _) _ x1) ->
        gotoComp s (Scomp stack s (reduction57 x0 x1))
run' S142 stack (TKeyword "=") = case stack of
    (Scomp (Skeyword (Sadd stack s x0) _) _ x1) ->
        gotoComp s (Scomp stack s (reduction57 x0 x1))
run' S142 stack (TKeyword "≡") = case stack of
    (Scomp (Skeyword (Sadd stack s x0) _) _ x1) ->
        gotoComp s (Scomp stack s (reduction57 x0 x1))
run' S142 stack (TKeyword ";") = case stack of
    (Scomp (Skeyword (Sadd stack s x0) _) _ x1) ->
        gotoComp s (Scomp stack s (reduction57 x0 x1))
run' S142 stack (TKeyword "⊔") = case stack of
    (Scomp (Skeyword (Sadd stack s x0) _) _ x1) ->
        gotoComp s (Scomp stack s (reduction57 x0 x1))
run' S142 stack (TKeyword "⟯") = case stack of
    (Scomp (Skeyword (Sadd stack s x0) _) _ x1) ->
        gotoComp s (Scomp stack s (reduction57 x0 x1))
run' S142 stack (TKeyword "→") = case stack of
    (Scomp (Skeyword (Sadd stack s x0) _) _ x1) ->
        gotoComp s (Scomp stack s (reduction57 x0 x1))
run' S142 stack (TKeyword ",") = case stack of
    (Scomp (Skeyword (Sadd stack s x0) _) _ x1) ->
        gotoComp s (Scomp stack s (reduction57 x0 x1))
run' S143 stack (TKeyword "⟧") = shift >> run S189 (Skeyword stack S143)
run' S144 stack (TKeyword "-") = case stack of
    (Sprim (Sstruct (Scall stack s x0) _ x1) _ x2) ->
        gotoCall s (Scall stack s (reduction69 x0 x1 x2))
run' S144 stack (TKeyword "[") = case stack of
    (Sprim (Sstruct (Scall stack s x0) _ x1) _ x2) ->
        gotoCall s (Scall stack s (reduction69 x0 x1 x2))
run' S144 stack (TKeyword "^") = case stack of
    (Sprim (Sstruct (Scall stack s x0) _ x1) _ x2) ->
        gotoCall s (Scall stack s (reduction69 x0 x1 x2))
run' S144 stack (TKeyword ")") = case stack of
    (Sprim (Sstruct (Scall stack s x0) _ x1) _ x2) ->
        gotoCall s (Scall stack s (reduction69 x0 x1 x2))
run' S144 stack (TKeyword "⟦") = case stack of
    (Sprim (Sstruct (Scall stack s x0) _ x1) _ x2) ->
        gotoCall s (Scall stack s (reduction69 x0 x1 x2))
run' S144 stack (TKeyword "≤") = case stack of
    (Sprim (Sstruct (Scall stack s x0) _ x1) _ x2) ->
        gotoCall s (Scall stack s (reduction69 x0 x1 x2))
run' S144 stack (TKeyword "<") = case stack of
    (Sprim (Sstruct (Scall stack s x0) _ x1) _ x2) ->
        gotoCall s (Scall stack s (reduction69 x0 x1 x2))
run' S144 stack (TKeyword "≠") = case stack of
    (Sprim (Sstruct (Scall stack s x0) _ x1) _ x2) ->
        gotoCall s (Scall stack s (reduction69 x0 x1 x2))
run' S144 stack (TKeyword "{") = case stack of
    (Sprim (Sstruct (Scall stack s x0) _ x1) _ x2) ->
        gotoCall s (Scall stack s (reduction69 x0 x1 x2))
run' S144 stack (TKeyword "⟮") = case stack of
    (Sprim (Sstruct (Scall stack s x0) _ x1) _ x2) ->
        gotoCall s (Scall stack s (reduction69 x0 x1 x2))
run' S144 stack (TReal arg) = case stack of
    (Sprim (Sstruct (Scall stack s x0) _ x1) _ x2) ->
        gotoCall s (Scall stack s (reduction69 x0 x1 x2))
run' S144 stack (TKeyword "⋆") = case stack of
    (Sprim (Sstruct (Scall stack s x0) _ x1) _ x2) ->
        gotoCall s (Scall stack s (reduction69 x0 x1 x2))
run' S144 stack (TKeyword "*") = case stack of
    (Sprim (Sstruct (Scall stack s x0) _ x1) _ x2) ->
        gotoCall s (Scall stack s (reduction69 x0 x1 x2))
run' S144 stack (TKeyword "def") = case stack of
    (Sprim (Sstruct (Scall stack s x0) _ x1) _ x2) ->
        gotoCall s (Scall stack s (reduction69 x0 x1 x2))
run' S144 stack TEnd = case stack of
    (Sprim (Sstruct (Scall stack s x0) _ x1) _ x2) ->
        gotoCall s (Scall stack s (reduction69 x0 x1 x2))
run' S144 stack (TInteger arg) = case stack of
    (Sprim (Sstruct (Scall stack s x0) _ x1) _ x2) ->
        gotoCall s (Scall stack s (reduction69 x0 x1 x2))
run' S144 stack (TKeyword "∘") = case stack of
    (Sprim (Sstruct (Scall stack s x0) _ x1) _ x2) ->
        gotoCall s (Scall stack s (reduction69 x0 x1 x2))
run' S144 stack (TKeyword "⟧") = case stack of
    (Sprim (Sstruct (Scall stack s x0) _ x1) _ x2) ->
        gotoCall s (Scall stack s (reduction69 x0 x1 x2))
run' S144 stack (TKeyword "≥") = case stack of
    (Sprim (Sstruct (Scall stack s x0) _ x1) _ x2) ->
        gotoCall s (Scall stack s (reduction69 x0 x1 x2))
run' S144 stack (TKeyword "|") = case stack of
    (Sprim (Sstruct (Scall stack s x0) _ x1) _ x2) ->
        gotoCall s (Scall stack s (reduction69 x0 x1 x2))
run' S144 stack (TKeyword "=") = case stack of
    (Sprim (Sstruct (Scall stack s x0) _ x1) _ x2) ->
        gotoCall s (Scall stack s (reduction69 x0 x1 x2))
run' S144 stack (TKeyword "≡") = case stack of
    (Sprim (Sstruct (Scall stack s x0) _ x1) _ x2) ->
        gotoCall s (Scall stack s (reduction69 x0 x1 x2))
run' S144 stack (TKeyword "⟯") = case stack of
    (Sprim (Sstruct (Scall stack s x0) _ x1) _ x2) ->
        gotoCall s (Scall stack s (reduction69 x0 x1 x2))
run' S144 stack (TKeyword "→") = case stack of
    (Sprim (Sstruct (Scall stack s x0) _ x1) _ x2) ->
        gotoCall s (Scall stack s (reduction69 x0 x1 x2))
run' S144 stack (TKeyword "+") = case stack of
    (Sprim (Sstruct (Scall stack s x0) _ x1) _ x2) ->
        gotoCall s (Scall stack s (reduction69 x0 x1 x2))
run' S144 stack (TKeyword "⟨") = case stack of
    (Sprim (Sstruct (Scall stack s x0) _ x1) _ x2) ->
        gotoCall s (Scall stack s (reduction69 x0 x1 x2))
run' S144 stack (TKeyword "export") = case stack of
    (Sprim (Sstruct (Scall stack s x0) _ x1) _ x2) ->
        gotoCall s (Scall stack s (reduction69 x0 x1 x2))
run' S144 stack (TId arg) = case stack of
    (Sprim (Sstruct (Scall stack s x0) _ x1) _ x2) ->
        gotoCall s (Scall stack s (reduction69 x0 x1 x2))
run' S144 stack (TString arg) = case stack of
    (Sprim (Sstruct (Scall stack s x0) _ x1) _ x2) ->
        gotoCall s (Scall stack s (reduction69 x0 x1 x2))
run' S144 stack (TKeyword "∧") = case stack of
    (Sprim (Sstruct (Scall stack s x0) _ x1) _ x2) ->
        gotoCall s (Scall stack s (reduction69 x0 x1 x2))
run' S144 stack (TKeyword ">") = case stack of
    (Sprim (Sstruct (Scall stack s x0) _ x1) _ x2) ->
        gotoCall s (Scall stack s (reduction69 x0 x1 x2))
run' S144 stack (TKeyword "}") = case stack of
    (Sprim (Sstruct (Scall stack s x0) _ x1) _ x2) ->
        gotoCall s (Scall stack s (reduction69 x0 x1 x2))
run' S144 stack (TKeyword ":") = case stack of
    (Sprim (Sstruct (Scall stack s x0) _ x1) _ x2) ->
        gotoCall s (Scall stack s (reduction69 x0 x1 x2))
run' S144 stack (TKeyword "/") = case stack of
    (Sprim (Sstruct (Scall stack s x0) _ x1) _ x2) ->
        gotoCall s (Scall stack s (reduction69 x0 x1 x2))
run' S144 stack (TKeyword "⊓") = case stack of
    (Sprim (Sstruct (Scall stack s x0) _ x1) _ x2) ->
        gotoCall s (Scall stack s (reduction69 x0 x1 x2))
run' S144 stack (TKeyword "(") = case stack of
    (Sprim (Sstruct (Scall stack s x0) _ x1) _ x2) ->
        gotoCall s (Scall stack s (reduction69 x0 x1 x2))
run' S144 stack (TKeyword "import") = case stack of
    (Sprim (Sstruct (Scall stack s x0) _ x1) _ x2) ->
        gotoCall s (Scall stack s (reduction69 x0 x1 x2))
run' S144 stack (TKeyword "∨") = case stack of
    (Sprim (Sstruct (Scall stack s x0) _ x1) _ x2) ->
        gotoCall s (Scall stack s (reduction69 x0 x1 x2))
run' S144 stack (TKeyword "⟩") = case stack of
    (Sprim (Sstruct (Scall stack s x0) _ x1) _ x2) ->
        gotoCall s (Scall stack s (reduction69 x0 x1 x2))
run' S144 stack (TKeyword "]") = case stack of
    (Sprim (Sstruct (Scall stack s x0) _ x1) _ x2) ->
        gotoCall s (Scall stack s (reduction69 x0 x1 x2))
run' S144 stack (TKeyword ";") = case stack of
    (Sprim (Sstruct (Scall stack s x0) _ x1) _ x2) ->
        gotoCall s (Scall stack s (reduction69 x0 x1 x2))
run' S144 stack (TKeyword "⊔") = case stack of
    (Sprim (Sstruct (Scall stack s x0) _ x1) _ x2) ->
        gotoCall s (Scall stack s (reduction69 x0 x1 x2))
run' S144 stack (TKeyword ",") = case stack of
    (Sprim (Sstruct (Scall stack s x0) _ x1) _ x2) ->
        gotoCall s (Scall stack s (reduction69 x0 x1 x2))
run' S145 stack (TKeyword "-") = case stack of
    (Sexp (Skeyword (Scall stack s x0) _) _ x1) ->
        gotoExp s (Sexp stack s (reduction67 x0 x1))
run' S145 stack (TKeyword "+") = case stack of
    (Sexp (Skeyword (Scall stack s x0) _) _ x1) ->
        gotoExp s (Sexp stack s (reduction67 x0 x1))
run' S145 stack (TKeyword ")") = case stack of
    (Sexp (Skeyword (Scall stack s x0) _) _ x1) ->
        gotoExp s (Sexp stack s (reduction67 x0 x1))
run' S145 stack (TKeyword "export") = case stack of
    (Sexp (Skeyword (Scall stack s x0) _) _ x1) ->
        gotoExp s (Sexp stack s (reduction67 x0 x1))
run' S145 stack (TKeyword "import") = case stack of
    (Sexp (Skeyword (Scall stack s x0) _) _ x1) ->
        gotoExp s (Sexp stack s (reduction67 x0 x1))
run' S145 stack (TKeyword "∧") = case stack of
    (Sexp (Skeyword (Scall stack s x0) _) _ x1) ->
        gotoExp s (Sexp stack s (reduction67 x0 x1))
run' S145 stack (TKeyword "≤") = case stack of
    (Sexp (Skeyword (Scall stack s x0) _) _ x1) ->
        gotoExp s (Sexp stack s (reduction67 x0 x1))
run' S145 stack (TKeyword ">") = case stack of
    (Sexp (Skeyword (Scall stack s x0) _) _ x1) ->
        gotoExp s (Sexp stack s (reduction67 x0 x1))
run' S145 stack (TKeyword "<") = case stack of
    (Sexp (Skeyword (Scall stack s x0) _) _ x1) ->
        gotoExp s (Sexp stack s (reduction67 x0 x1))
run' S145 stack (TKeyword "}") = case stack of
    (Sexp (Skeyword (Scall stack s x0) _) _ x1) ->
        gotoExp s (Sexp stack s (reduction67 x0 x1))
run' S145 stack (TKeyword "≠") = case stack of
    (Sexp (Skeyword (Scall stack s x0) _) _ x1) ->
        gotoExp s (Sexp stack s (reduction67 x0 x1))
run' S145 stack (TKeyword ":") = case stack of
    (Sexp (Skeyword (Scall stack s x0) _) _ x1) ->
        gotoExp s (Sexp stack s (reduction67 x0 x1))
run' S145 stack (TKeyword "/") = case stack of
    (Sexp (Skeyword (Scall stack s x0) _) _ x1) ->
        gotoExp s (Sexp stack s (reduction67 x0 x1))
run' S145 stack (TKeyword "⊓") = case stack of
    (Sexp (Skeyword (Scall stack s x0) _) _ x1) ->
        gotoExp s (Sexp stack s (reduction67 x0 x1))
run' S145 stack (TKeyword "*") = case stack of
    (Sexp (Skeyword (Scall stack s x0) _) _ x1) ->
        gotoExp s (Sexp stack s (reduction67 x0 x1))
run' S145 stack (TKeyword "def") = case stack of
    (Sexp (Skeyword (Scall stack s x0) _) _ x1) ->
        gotoExp s (Sexp stack s (reduction67 x0 x1))
run' S145 stack (TKeyword "∨") = case stack of
    (Sexp (Skeyword (Scall stack s x0) _) _ x1) ->
        gotoExp s (Sexp stack s (reduction67 x0 x1))
run' S145 stack (TKeyword "⟩") = case stack of
    (Sexp (Skeyword (Scall stack s x0) _) _ x1) ->
        gotoExp s (Sexp stack s (reduction67 x0 x1))
run' S145 stack (TKeyword "]") = case stack of
    (Sexp (Skeyword (Scall stack s x0) _) _ x1) ->
        gotoExp s (Sexp stack s (reduction67 x0 x1))
run' S145 stack TEnd = case stack of
    (Sexp (Skeyword (Scall stack s x0) _) _ x1) ->
        gotoExp s (Sexp stack s (reduction67 x0 x1))
run' S145 stack (TKeyword "∘") = case stack of
    (Sexp (Skeyword (Scall stack s x0) _) _ x1) ->
        gotoExp s (Sexp stack s (reduction67 x0 x1))
run' S145 stack (TKeyword "⟧") = case stack of
    (Sexp (Skeyword (Scall stack s x0) _) _ x1) ->
        gotoExp s (Sexp stack s (reduction67 x0 x1))
run' S145 stack (TKeyword "≥") = case stack of
    (Sexp (Skeyword (Scall stack s x0) _) _ x1) ->
        gotoExp s (Sexp stack s (reduction67 x0 x1))
run' S145 stack (TKeyword "|") = case stack of
    (Sexp (Skeyword (Scall stack s x0) _) _ x1) ->
        gotoExp s (Sexp stack s (reduction67 x0 x1))
run' S145 stack (TKeyword "=") = case stack of
    (Sexp (Skeyword (Scall stack s x0) _) _ x1) ->
        gotoExp s (Sexp stack s (reduction67 x0 x1))
run' S145 stack (TKeyword "≡") = case stack of
    (Sexp (Skeyword (Scall stack s x0) _) _ x1) ->
        gotoExp s (Sexp stack s (reduction67 x0 x1))
run' S145 stack (TKeyword ";") = case stack of
    (Sexp (Skeyword (Scall stack s x0) _) _ x1) ->
        gotoExp s (Sexp stack s (reduction67 x0 x1))
run' S145 stack (TKeyword "⊔") = case stack of
    (Sexp (Skeyword (Scall stack s x0) _) _ x1) ->
        gotoExp s (Sexp stack s (reduction67 x0 x1))
run' S145 stack (TKeyword "⟯") = case stack of
    (Sexp (Skeyword (Scall stack s x0) _) _ x1) ->
        gotoExp s (Sexp stack s (reduction67 x0 x1))
run' S145 stack (TKeyword "→") = case stack of
    (Sexp (Skeyword (Scall stack s x0) _) _ x1) ->
        gotoExp s (Sexp stack s (reduction67 x0 x1))
run' S145 stack (TKeyword ",") = case stack of
    (Sexp (Skeyword (Scall stack s x0) _) _ x1) ->
        gotoExp s (Sexp stack s (reduction67 x0 x1))
run' S146 stack (TKeyword "⟩") = shift >> run S190 (Skeyword stack S146)
run' S147 stack (TReal arg) = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoStruct s (Sstruct stack s (reduction86 ))
run' S147 stack (TKeyword "⋆") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoStruct s (Sstruct stack s (reduction86 ))
run' S147 stack (TKeyword "{") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoStruct s (Sstruct stack s (reduction86 ))
run' S147 stack (TKeyword "[") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoStruct s (Sstruct stack s (reduction86 ))
run' S147 stack (TInteger arg) = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoStruct s (Sstruct stack s (reduction86 ))
run' S147 stack (TKeyword "(") = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoStruct s (Sstruct stack s (reduction86 ))
run' S147 stack (TId arg) = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoStruct s (Sstruct stack s (reduction86 ))
run' S147 stack (TString arg) = case stack of
    (Skeyword (Skeyword stack s) _) ->
        gotoStruct s (Sstruct stack s (reduction86 ))
run' S148 stack (TKeyword "⟩") = shift >> run S191 (Skeyword stack S148)
run' S149 stack (TKeyword "⟯") = shift >> run S192 (Skeyword stack S149)
run' S150 stack (TKeyword "-") = case stack of
    (Skeyword (Slabterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction80 x0))
run' S150 stack (TKeyword "[") = case stack of
    (Skeyword (Slabterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction80 x0))
run' S150 stack (TKeyword "^") = case stack of
    (Skeyword (Slabterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction80 x0))
run' S150 stack (TKeyword ")") = case stack of
    (Skeyword (Slabterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction80 x0))
run' S150 stack (TKeyword "⟦") = case stack of
    (Skeyword (Slabterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction80 x0))
run' S150 stack (TKeyword "≤") = case stack of
    (Skeyword (Slabterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction80 x0))
run' S150 stack (TKeyword "<") = case stack of
    (Skeyword (Slabterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction80 x0))
run' S150 stack (TKeyword "≠") = case stack of
    (Skeyword (Slabterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction80 x0))
run' S150 stack (TKeyword "{") = case stack of
    (Skeyword (Slabterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction80 x0))
run' S150 stack (TKeyword "⟮") = case stack of
    (Skeyword (Slabterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction80 x0))
run' S150 stack (TReal arg) = case stack of
    (Skeyword (Slabterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction80 x0))
run' S150 stack (TKeyword "⋆") = case stack of
    (Skeyword (Slabterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction80 x0))
run' S150 stack (TKeyword "*") = case stack of
    (Skeyword (Slabterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction80 x0))
run' S150 stack (TKeyword "def") = case stack of
    (Skeyword (Slabterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction80 x0))
run' S150 stack TEnd = case stack of
    (Skeyword (Slabterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction80 x0))
run' S150 stack (TInteger arg) = case stack of
    (Skeyword (Slabterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction80 x0))
run' S150 stack (TKeyword "∘") = case stack of
    (Skeyword (Slabterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction80 x0))
run' S150 stack (TKeyword "⟧") = case stack of
    (Skeyword (Slabterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction80 x0))
run' S150 stack (TKeyword "≥") = case stack of
    (Skeyword (Slabterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction80 x0))
run' S150 stack (TKeyword "|") = case stack of
    (Skeyword (Slabterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction80 x0))
run' S150 stack (TKeyword "=") = case stack of
    (Skeyword (Slabterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction80 x0))
run' S150 stack (TKeyword "≡") = case stack of
    (Skeyword (Slabterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction80 x0))
run' S150 stack (TKeyword "⟯") = case stack of
    (Skeyword (Slabterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction80 x0))
run' S150 stack (TKeyword "→") = case stack of
    (Skeyword (Slabterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction80 x0))
run' S150 stack (TKeyword "+") = case stack of
    (Skeyword (Slabterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction80 x0))
run' S150 stack (TKeyword "⟨") = case stack of
    (Skeyword (Slabterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction80 x0))
run' S150 stack (TKeyword "export") = case stack of
    (Skeyword (Slabterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction80 x0))
run' S150 stack (TId arg) = case stack of
    (Skeyword (Slabterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction80 x0))
run' S150 stack (TString arg) = case stack of
    (Skeyword (Slabterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction80 x0))
run' S150 stack (TKeyword "∧") = case stack of
    (Skeyword (Slabterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction80 x0))
run' S150 stack (TKeyword ">") = case stack of
    (Skeyword (Slabterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction80 x0))
run' S150 stack (TKeyword "}") = case stack of
    (Skeyword (Slabterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction80 x0))
run' S150 stack (TKeyword ":") = case stack of
    (Skeyword (Slabterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction80 x0))
run' S150 stack (TKeyword "/") = case stack of
    (Skeyword (Slabterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction80 x0))
run' S150 stack (TKeyword "⊓") = case stack of
    (Skeyword (Slabterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction80 x0))
run' S150 stack (TKeyword "(") = case stack of
    (Skeyword (Slabterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction80 x0))
run' S150 stack (TKeyword "import") = case stack of
    (Skeyword (Slabterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction80 x0))
run' S150 stack (TKeyword "∨") = case stack of
    (Skeyword (Slabterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction80 x0))
run' S150 stack (TKeyword "⟩") = case stack of
    (Skeyword (Slabterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction80 x0))
run' S150 stack (TKeyword "]") = case stack of
    (Skeyword (Slabterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction80 x0))
run' S150 stack (TKeyword ";") = case stack of
    (Skeyword (Slabterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction80 x0))
run' S150 stack (TKeyword "⊔") = case stack of
    (Skeyword (Slabterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction80 x0))
run' S150 stack (TKeyword ",") = case stack of
    (Skeyword (Slabterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction80 x0))
run' S151 stack (TKeyword "-") = shift >> run S28 (Skeyword stack S151)
run' S151 stack (TKeyword "⋆") = shift >> run S45 (Skeyword stack S151)
run' S151 stack (TKeyword "⟨") = shift >> run S31 (Skeyword stack S151)
run' S151 stack (TKeyword "λ") = shift >> run S46 (Skeyword stack S151)
run' S151 stack (TKeyword "(") = shift >> run S49 (Skeyword stack S151)
run' S151 stack (TKeyword "∃") = shift >> run S52 (Skeyword stack S151)
run' S151 stack (TKeyword "[") = shift >> run S54 (Skeyword stack S151)
run' S151 stack (TInteger arg) = shift >> run S57 (Sinteger stack S151 arg)
run' S151 stack (TString arg) = shift >> run S36 (Sstring stack S151 arg)
run' S151 stack (TKeyword "⟦") = shift >> run S37 (Skeyword stack S151)
run' S151 stack (TKeyword "~") = shift >> run S59 (Skeyword stack S151)
run' S151 stack (TId arg) = shift >> run S39 (Sid stack S151 arg)
run' S151 stack (TKeyword "{") = shift >> run S42 (Skeyword stack S151)
run' S151 stack (TReal arg) = shift >> run S44 (Sreal stack S151 arg)
run' S151 stack (TKeyword "⟮") = shift >> run S43 (Skeyword stack S151)
run' S151 stack (TKeyword "¬") = shift >> run S63 (Skeyword stack S151)
run' S152 stack (TKeyword "-") = shift >> run S28 (Skeyword stack S152)
run' S152 stack (TKeyword "⋆") = shift >> run S45 (Skeyword stack S152)
run' S152 stack (TKeyword "⟨") = shift >> run S31 (Skeyword stack S152)
run' S152 stack (TKeyword "λ") = shift >> run S46 (Skeyword stack S152)
run' S152 stack (TKeyword "(") = shift >> run S49 (Skeyword stack S152)
run' S152 stack (TKeyword "∃") = shift >> run S52 (Skeyword stack S152)
run' S152 stack (TKeyword "[") = shift >> run S54 (Skeyword stack S152)
run' S152 stack (TInteger arg) = shift >> run S57 (Sinteger stack S152 arg)
run' S152 stack (TString arg) = shift >> run S36 (Sstring stack S152 arg)
run' S152 stack (TKeyword "⟦") = shift >> run S37 (Skeyword stack S152)
run' S152 stack (TKeyword "~") = shift >> run S59 (Skeyword stack S152)
run' S152 stack (TId arg) = shift >> run S39 (Sid stack S152 arg)
run' S152 stack (TKeyword "{") = shift >> run S42 (Skeyword stack S152)
run' S152 stack (TReal arg) = shift >> run S44 (Sreal stack S152 arg)
run' S152 stack (TKeyword "⟮") = shift >> run S43 (Skeyword stack S152)
run' S152 stack (TKeyword "¬") = shift >> run S63 (Skeyword stack S152)
run' S153 stack (TKeyword "-") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction79 x0))
run' S153 stack (TKeyword "[") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction79 x0))
run' S153 stack (TKeyword "^") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction79 x0))
run' S153 stack (TKeyword ")") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction79 x0))
run' S153 stack (TKeyword "⟦") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction79 x0))
run' S153 stack (TKeyword "≤") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction79 x0))
run' S153 stack (TKeyword "<") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction79 x0))
run' S153 stack (TKeyword "≠") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction79 x0))
run' S153 stack (TKeyword "{") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction79 x0))
run' S153 stack (TKeyword "⟮") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction79 x0))
run' S153 stack (TReal arg) = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction79 x0))
run' S153 stack (TKeyword "⋆") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction79 x0))
run' S153 stack (TKeyword "*") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction79 x0))
run' S153 stack (TKeyword "def") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction79 x0))
run' S153 stack TEnd = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction79 x0))
run' S153 stack (TInteger arg) = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction79 x0))
run' S153 stack (TKeyword "∘") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction79 x0))
run' S153 stack (TKeyword "⟧") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction79 x0))
run' S153 stack (TKeyword "≥") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction79 x0))
run' S153 stack (TKeyword "|") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction79 x0))
run' S153 stack (TKeyword "=") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction79 x0))
run' S153 stack (TKeyword "≡") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction79 x0))
run' S153 stack (TKeyword "⟯") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction79 x0))
run' S153 stack (TKeyword "→") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction79 x0))
run' S153 stack (TKeyword "+") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction79 x0))
run' S153 stack (TKeyword "⟨") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction79 x0))
run' S153 stack (TKeyword "export") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction79 x0))
run' S153 stack (TId arg) = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction79 x0))
run' S153 stack (TString arg) = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction79 x0))
run' S153 stack (TKeyword "∧") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction79 x0))
run' S153 stack (TKeyword ">") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction79 x0))
run' S153 stack (TKeyword "}") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction79 x0))
run' S153 stack (TKeyword ":") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction79 x0))
run' S153 stack (TKeyword "/") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction79 x0))
run' S153 stack (TKeyword "⊓") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction79 x0))
run' S153 stack (TKeyword "(") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction79 x0))
run' S153 stack (TKeyword "import") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction79 x0))
run' S153 stack (TKeyword "∨") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction79 x0))
run' S153 stack (TKeyword "⟩") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction79 x0))
run' S153 stack (TKeyword "]") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction79 x0))
run' S153 stack (TKeyword ";") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction79 x0))
run' S153 stack (TKeyword "⊔") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction79 x0))
run' S153 stack (TKeyword ",") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction79 x0))
run' S154 stack (TId arg) = shift >> run S195 (Sid stack S154 arg)
run' S155 stack (TReal arg) = shift >> run S44 (Sreal stack S155 arg)
run' S155 stack (TKeyword "⋆") = shift >> run S45 (Skeyword stack S155)
run' S155 stack (TKeyword "λ") = shift >> run S46 (Skeyword stack S155)
run' S155 stack (TKeyword "(") = shift >> run S96 (Skeyword stack S155)
run' S155 stack (TKeyword "∃") = shift >> run S52 (Skeyword stack S155)
run' S155 stack (TKeyword "[") = shift >> run S54 (Skeyword stack S155)
run' S155 stack (TInteger arg) = shift >> run S57 (Sinteger stack S155 arg)
run' S155 stack (TString arg) = shift >> run S36 (Sstring stack S155 arg)
run' S155 stack (TKeyword "~") = shift >> run S59 (Skeyword stack S155)
run' S155 stack (TId arg) = shift >> run S39 (Sid stack S155 arg)
run' S155 stack (TKeyword "{") = shift >> run S42 (Skeyword stack S155)
run' S155 stack (TKeyword "⟮") = shift >> run S43 (Skeyword stack S155)
run' S155 stack (TKeyword "¬") = shift >> run S63 (Skeyword stack S155)
run' S156 stack (TId arg) = shift >> run S101 (Sid stack S156 arg)
run' S157 stack (TKeyword "import") = case stack of
    (Skeyword (Scases (Skeyword stack s) _ x0) _) ->
        gotoCocase s (Scocase stack s (reduction38 x0))
run' S157 stack (TKeyword "⟧") = case stack of
    (Skeyword (Scases (Skeyword stack s) _ x0) _) ->
        gotoCocase s (Scocase stack s (reduction38 x0))
run' S157 stack (TKeyword ";") = case stack of
    (Skeyword (Scases (Skeyword stack s) _ x0) _) ->
        gotoCocase s (Scocase stack s (reduction38 x0))
run' S157 stack (TKeyword "⟩") = case stack of
    (Skeyword (Scases (Skeyword stack s) _ x0) _) ->
        gotoCocase s (Scocase stack s (reduction38 x0))
run' S157 stack (TKeyword "def") = case stack of
    (Skeyword (Scases (Skeyword stack s) _ x0) _) ->
        gotoCocase s (Scocase stack s (reduction38 x0))
run' S157 stack (TKeyword "export") = case stack of
    (Skeyword (Scases (Skeyword stack s) _ x0) _) ->
        gotoCocase s (Scocase stack s (reduction38 x0))
run' S157 stack (TKeyword ")") = case stack of
    (Skeyword (Scases (Skeyword stack s) _ x0) _) ->
        gotoCocase s (Scocase stack s (reduction38 x0))
run' S157 stack (TKeyword "|") = case stack of
    (Skeyword (Scases (Skeyword stack s) _ x0) _) ->
        gotoCocase s (Scocase stack s (reduction38 x0))
run' S157 stack (TKeyword "]") = case stack of
    (Skeyword (Scases (Skeyword stack s) _ x0) _) ->
        gotoCocase s (Scocase stack s (reduction38 x0))
run' S157 stack (TKeyword "}") = case stack of
    (Skeyword (Scases (Skeyword stack s) _ x0) _) ->
        gotoCocase s (Scocase stack s (reduction38 x0))
run' S157 stack (TKeyword "≡") = case stack of
    (Skeyword (Scases (Skeyword stack s) _ x0) _) ->
        gotoCocase s (Scocase stack s (reduction38 x0))
run' S157 stack (TKeyword ":") = case stack of
    (Skeyword (Scases (Skeyword stack s) _ x0) _) ->
        gotoCocase s (Scocase stack s (reduction38 x0))
run' S157 stack TEnd = case stack of
    (Skeyword (Scases (Skeyword stack s) _ x0) _) ->
        gotoCocase s (Scocase stack s (reduction38 x0))
run' S157 stack (TKeyword "⟯") = case stack of
    (Skeyword (Scases (Skeyword stack s) _ x0) _) ->
        gotoCocase s (Scocase stack s (reduction38 x0))
run' S157 stack (TKeyword "→") = case stack of
    (Skeyword (Scases (Skeyword stack s) _ x0) _) ->
        gotoCocase s (Scocase stack s (reduction38 x0))
run' S157 stack (TKeyword ",") = case stack of
    (Skeyword (Scases (Skeyword stack s) _ x0) _) ->
        gotoCocase s (Scocase stack s (reduction38 x0))
run' S158 stack (TKeyword "⇒") = case stack of
    (Sbinders stack s x0) ->
        gotoBinders0 s (Sbinders0 stack s (reduction107 x0))
run' S159 stack (TKeyword "⇒") = shift >> run S199 (Skeyword stack S159)
run' S160 stack (TId arg) = shift >> run S39 (Sid stack S160 arg)
run' S160 stack (TString arg) = shift >> run S36 (Sstring stack S160 arg)
run' S160 stack (TKeyword "{") = shift >> run S42 (Skeyword stack S160)
run' S160 stack (TKeyword "⟮") = shift >> run S43 (Skeyword stack S160)
run' S160 stack (TReal arg) = shift >> run S44 (Sreal stack S160 arg)
run' S160 stack (TKeyword "⋆") = shift >> run S45 (Skeyword stack S160)
run' S160 stack (TKeyword "λ") = shift >> run S46 (Skeyword stack S160)
run' S160 stack (TKeyword "(") = shift >> run S96 (Skeyword stack S160)
run' S160 stack (TKeyword "∃") = shift >> run S52 (Skeyword stack S160)
run' S160 stack (TKeyword "[") = shift >> run S54 (Skeyword stack S160)
run' S160 stack (TInteger arg) = shift >> run S57 (Sinteger stack S160 arg)
run' S160 stack (TKeyword "~") = shift >> run S59 (Skeyword stack S160)
run' S160 stack (TKeyword "¬") = shift >> run S63 (Skeyword stack S160)
run' S161 stack (TKeyword "import") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction52 x0 x1))
run' S161 stack (TKeyword "∧") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction52 x0 x1))
run' S161 stack (TKeyword "∨") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction52 x0 x1))
run' S161 stack (TKeyword ";") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction52 x0 x1))
run' S161 stack (TKeyword "⟩") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction52 x0 x1))
run' S161 stack (TKeyword "def") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction52 x0 x1))
run' S161 stack (TKeyword "export") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction52 x0 x1))
run' S161 stack (TKeyword ")") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction52 x0 x1))
run' S161 stack (TKeyword "⟧") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction52 x0 x1))
run' S161 stack (TKeyword "|") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction52 x0 x1))
run' S161 stack (TKeyword "]") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction52 x0 x1))
run' S161 stack (TKeyword "}") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction52 x0 x1))
run' S161 stack (TKeyword "≡") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction52 x0 x1))
run' S161 stack (TKeyword ":") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction52 x0 x1))
run' S161 stack TEnd = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction52 x0 x1))
run' S161 stack (TKeyword "⊔") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction52 x0 x1))
run' S161 stack (TKeyword "⟯") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction52 x0 x1))
run' S161 stack (TKeyword "→") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction52 x0 x1))
run' S161 stack (TKeyword "⊓") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction52 x0 x1))
run' S161 stack (TKeyword ",") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction52 x0 x1))
run' S162 stack (TKeyword "import") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction53 x0 x1))
run' S162 stack (TKeyword "∧") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction53 x0 x1))
run' S162 stack (TKeyword "∨") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction53 x0 x1))
run' S162 stack (TKeyword ";") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction53 x0 x1))
run' S162 stack (TKeyword "⟩") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction53 x0 x1))
run' S162 stack (TKeyword "def") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction53 x0 x1))
run' S162 stack (TKeyword "export") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction53 x0 x1))
run' S162 stack (TKeyword ")") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction53 x0 x1))
run' S162 stack (TKeyword "⟧") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction53 x0 x1))
run' S162 stack (TKeyword "|") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction53 x0 x1))
run' S162 stack (TKeyword "]") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction53 x0 x1))
run' S162 stack (TKeyword "}") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction53 x0 x1))
run' S162 stack (TKeyword "≡") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction53 x0 x1))
run' S162 stack (TKeyword ":") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction53 x0 x1))
run' S162 stack TEnd = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction53 x0 x1))
run' S162 stack (TKeyword "⊔") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction53 x0 x1))
run' S162 stack (TKeyword "⟯") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction53 x0 x1))
run' S162 stack (TKeyword "→") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction53 x0 x1))
run' S162 stack (TKeyword "⊓") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction53 x0 x1))
run' S162 stack (TKeyword ",") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction53 x0 x1))
run' S163 stack (TKeyword "import") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction51 x0 x1))
run' S163 stack (TKeyword "∧") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction51 x0 x1))
run' S163 stack (TKeyword "∨") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction51 x0 x1))
run' S163 stack (TKeyword ";") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction51 x0 x1))
run' S163 stack (TKeyword "⟩") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction51 x0 x1))
run' S163 stack (TKeyword "def") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction51 x0 x1))
run' S163 stack (TKeyword "export") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction51 x0 x1))
run' S163 stack (TKeyword ")") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction51 x0 x1))
run' S163 stack (TKeyword "⟧") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction51 x0 x1))
run' S163 stack (TKeyword "|") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction51 x0 x1))
run' S163 stack (TKeyword "]") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction51 x0 x1))
run' S163 stack (TKeyword "}") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction51 x0 x1))
run' S163 stack (TKeyword "≡") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction51 x0 x1))
run' S163 stack (TKeyword ":") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction51 x0 x1))
run' S163 stack TEnd = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction51 x0 x1))
run' S163 stack (TKeyword "⊔") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction51 x0 x1))
run' S163 stack (TKeyword "⟯") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction51 x0 x1))
run' S163 stack (TKeyword "→") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction51 x0 x1))
run' S163 stack (TKeyword "⊓") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction51 x0 x1))
run' S163 stack (TKeyword ",") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction51 x0 x1))
run' S164 stack (TKeyword "import") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction50 x0 x1))
run' S164 stack (TKeyword "∧") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction50 x0 x1))
run' S164 stack (TKeyword "∨") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction50 x0 x1))
run' S164 stack (TKeyword ";") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction50 x0 x1))
run' S164 stack (TKeyword "⟩") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction50 x0 x1))
run' S164 stack (TKeyword "def") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction50 x0 x1))
run' S164 stack (TKeyword "export") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction50 x0 x1))
run' S164 stack (TKeyword ")") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction50 x0 x1))
run' S164 stack (TKeyword "⟧") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction50 x0 x1))
run' S164 stack (TKeyword "|") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction50 x0 x1))
run' S164 stack (TKeyword "]") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction50 x0 x1))
run' S164 stack (TKeyword "}") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction50 x0 x1))
run' S164 stack (TKeyword "≡") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction50 x0 x1))
run' S164 stack (TKeyword ":") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction50 x0 x1))
run' S164 stack TEnd = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction50 x0 x1))
run' S164 stack (TKeyword "⊔") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction50 x0 x1))
run' S164 stack (TKeyword "⟯") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction50 x0 x1))
run' S164 stack (TKeyword "→") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction50 x0 x1))
run' S164 stack (TKeyword "⊓") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction50 x0 x1))
run' S164 stack (TKeyword ",") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction50 x0 x1))
run' S165 stack (TKeyword "import") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction54 x0 x1))
run' S165 stack (TKeyword "∧") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction54 x0 x1))
run' S165 stack (TKeyword "∨") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction54 x0 x1))
run' S165 stack (TKeyword ";") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction54 x0 x1))
run' S165 stack (TKeyword "⟩") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction54 x0 x1))
run' S165 stack (TKeyword "def") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction54 x0 x1))
run' S165 stack (TKeyword "export") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction54 x0 x1))
run' S165 stack (TKeyword ")") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction54 x0 x1))
run' S165 stack (TKeyword "⟧") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction54 x0 x1))
run' S165 stack (TKeyword "|") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction54 x0 x1))
run' S165 stack (TKeyword "]") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction54 x0 x1))
run' S165 stack (TKeyword "}") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction54 x0 x1))
run' S165 stack (TKeyword "≡") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction54 x0 x1))
run' S165 stack (TKeyword ":") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction54 x0 x1))
run' S165 stack TEnd = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction54 x0 x1))
run' S165 stack (TKeyword "⊔") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction54 x0 x1))
run' S165 stack (TKeyword "⟯") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction54 x0 x1))
run' S165 stack (TKeyword "→") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction54 x0 x1))
run' S165 stack (TKeyword "⊓") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction54 x0 x1))
run' S165 stack (TKeyword ",") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction54 x0 x1))
run' S166 stack (TKeyword "import") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction55 x0 x1))
run' S166 stack (TKeyword "∧") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction55 x0 x1))
run' S166 stack (TKeyword "∨") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction55 x0 x1))
run' S166 stack (TKeyword ";") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction55 x0 x1))
run' S166 stack (TKeyword "⟩") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction55 x0 x1))
run' S166 stack (TKeyword "def") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction55 x0 x1))
run' S166 stack (TKeyword "export") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction55 x0 x1))
run' S166 stack (TKeyword ")") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction55 x0 x1))
run' S166 stack (TKeyword "⟧") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction55 x0 x1))
run' S166 stack (TKeyword "|") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction55 x0 x1))
run' S166 stack (TKeyword "]") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction55 x0 x1))
run' S166 stack (TKeyword "}") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction55 x0 x1))
run' S166 stack (TKeyword "≡") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction55 x0 x1))
run' S166 stack (TKeyword ":") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction55 x0 x1))
run' S166 stack TEnd = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction55 x0 x1))
run' S166 stack (TKeyword "⊔") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction55 x0 x1))
run' S166 stack (TKeyword "⟯") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction55 x0 x1))
run' S166 stack (TKeyword "→") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction55 x0 x1))
run' S166 stack (TKeyword "⊓") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction55 x0 x1))
run' S166 stack (TKeyword ",") = case stack of
    (Scomp (Skeyword (Scomp stack s x0) _) _ x1) ->
        gotoEquals s (Sequals stack s (reduction55 x0 x1))
run' S167 stack (TKeyword "-") = shift >> run S28 (Skeyword stack S167)
run' S167 stack (TKeyword "⋆") = shift >> run S45 (Skeyword stack S167)
run' S167 stack (TKeyword "⟨") = shift >> run S31 (Skeyword stack S167)
run' S167 stack (TKeyword "λ") = shift >> run S46 (Skeyword stack S167)
run' S167 stack (TKeyword "(") = shift >> run S49 (Skeyword stack S167)
run' S167 stack (TKeyword "∃") = shift >> run S52 (Skeyword stack S167)
run' S167 stack (TKeyword "[") = shift >> run S54 (Skeyword stack S167)
run' S167 stack (TInteger arg) = shift >> run S57 (Sinteger stack S167 arg)
run' S167 stack (TString arg) = shift >> run S36 (Sstring stack S167 arg)
run' S167 stack (TKeyword "⟦") = shift >> run S37 (Skeyword stack S167)
run' S167 stack (TKeyword "~") = shift >> run S59 (Skeyword stack S167)
run' S167 stack (TId arg) = shift >> run S39 (Sid stack S167 arg)
run' S167 stack (TKeyword "{") = shift >> run S42 (Skeyword stack S167)
run' S167 stack (TReal arg) = shift >> run S44 (Sreal stack S167 arg)
run' S167 stack (TKeyword "⟮") = shift >> run S43 (Skeyword stack S167)
run' S167 stack (TKeyword "¬") = shift >> run S63 (Skeyword stack S167)
run' S168 stack (TKeyword "-") = case stack of
    (Skeyword (Snormal (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction73 x0))
run' S168 stack (TKeyword "[") = case stack of
    (Skeyword (Snormal (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction73 x0))
run' S168 stack (TKeyword "^") = case stack of
    (Skeyword (Snormal (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction73 x0))
run' S168 stack (TKeyword ")") = case stack of
    (Skeyword (Snormal (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction73 x0))
run' S168 stack (TKeyword "⟦") = case stack of
    (Skeyword (Snormal (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction73 x0))
run' S168 stack (TKeyword "≤") = case stack of
    (Skeyword (Snormal (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction73 x0))
run' S168 stack (TKeyword "<") = case stack of
    (Skeyword (Snormal (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction73 x0))
run' S168 stack (TKeyword "≠") = case stack of
    (Skeyword (Snormal (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction73 x0))
run' S168 stack (TKeyword "{") = case stack of
    (Skeyword (Snormal (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction73 x0))
run' S168 stack (TKeyword "⟮") = case stack of
    (Skeyword (Snormal (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction73 x0))
run' S168 stack (TReal arg) = case stack of
    (Skeyword (Snormal (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction73 x0))
run' S168 stack (TKeyword "⋆") = case stack of
    (Skeyword (Snormal (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction73 x0))
run' S168 stack (TKeyword "*") = case stack of
    (Skeyword (Snormal (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction73 x0))
run' S168 stack (TKeyword "def") = case stack of
    (Skeyword (Snormal (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction73 x0))
run' S168 stack TEnd = case stack of
    (Skeyword (Snormal (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction73 x0))
run' S168 stack (TInteger arg) = case stack of
    (Skeyword (Snormal (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction73 x0))
run' S168 stack (TKeyword "∘") = case stack of
    (Skeyword (Snormal (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction73 x0))
run' S168 stack (TKeyword "⟧") = case stack of
    (Skeyword (Snormal (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction73 x0))
run' S168 stack (TKeyword "≥") = case stack of
    (Skeyword (Snormal (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction73 x0))
run' S168 stack (TKeyword "|") = case stack of
    (Skeyword (Snormal (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction73 x0))
run' S168 stack (TKeyword "=") = case stack of
    (Skeyword (Snormal (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction73 x0))
run' S168 stack (TKeyword "≡") = case stack of
    (Skeyword (Snormal (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction73 x0))
run' S168 stack (TKeyword "⟯") = case stack of
    (Skeyword (Snormal (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction73 x0))
run' S168 stack (TKeyword "→") = case stack of
    (Skeyword (Snormal (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction73 x0))
run' S168 stack (TKeyword "+") = case stack of
    (Skeyword (Snormal (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction73 x0))
run' S168 stack (TKeyword "⟨") = case stack of
    (Skeyword (Snormal (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction73 x0))
run' S168 stack (TKeyword "export") = case stack of
    (Skeyword (Snormal (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction73 x0))
run' S168 stack (TId arg) = case stack of
    (Skeyword (Snormal (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction73 x0))
run' S168 stack (TString arg) = case stack of
    (Skeyword (Snormal (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction73 x0))
run' S168 stack (TKeyword "∧") = case stack of
    (Skeyword (Snormal (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction73 x0))
run' S168 stack (TKeyword ">") = case stack of
    (Skeyword (Snormal (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction73 x0))
run' S168 stack (TKeyword "}") = case stack of
    (Skeyword (Snormal (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction73 x0))
run' S168 stack (TKeyword ":") = case stack of
    (Skeyword (Snormal (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction73 x0))
run' S168 stack (TKeyword "/") = case stack of
    (Skeyword (Snormal (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction73 x0))
run' S168 stack (TKeyword "⊓") = case stack of
    (Skeyword (Snormal (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction73 x0))
run' S168 stack (TKeyword "(") = case stack of
    (Skeyword (Snormal (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction73 x0))
run' S168 stack (TKeyword "import") = case stack of
    (Skeyword (Snormal (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction73 x0))
run' S168 stack (TKeyword "∨") = case stack of
    (Skeyword (Snormal (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction73 x0))
run' S168 stack (TKeyword "⟩") = case stack of
    (Skeyword (Snormal (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction73 x0))
run' S168 stack (TKeyword "]") = case stack of
    (Skeyword (Snormal (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction73 x0))
run' S168 stack (TKeyword ";") = case stack of
    (Skeyword (Snormal (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction73 x0))
run' S168 stack (TKeyword "⊔") = case stack of
    (Skeyword (Snormal (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction73 x0))
run' S168 stack (TKeyword ",") = case stack of
    (Skeyword (Snormal (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction73 x0))
run' S169 stack (TId arg) = shift >> run S39 (Sid stack S169 arg)
run' S169 stack (TString arg) = shift >> run S36 (Sstring stack S169 arg)
run' S169 stack (TKeyword "{") = shift >> run S42 (Skeyword stack S169)
run' S169 stack (TKeyword "⟮") = shift >> run S43 (Skeyword stack S169)
run' S169 stack (TReal arg) = shift >> run S44 (Sreal stack S169 arg)
run' S169 stack (TKeyword "⋆") = shift >> run S45 (Skeyword stack S169)
run' S169 stack (TKeyword "λ") = shift >> run S46 (Skeyword stack S169)
run' S169 stack (TKeyword "(") = shift >> run S96 (Skeyword stack S169)
run' S169 stack (TKeyword "∃") = shift >> run S52 (Skeyword stack S169)
run' S169 stack (TKeyword "[") = shift >> run S54 (Skeyword stack S169)
run' S169 stack (TInteger arg) = shift >> run S57 (Sinteger stack S169 arg)
run' S169 stack (TKeyword "~") = shift >> run S59 (Skeyword stack S169)
run' S169 stack (TKeyword "¬") = shift >> run S63 (Skeyword stack S169)
run' S170 stack (TKeyword ".") = case stack of
    (Sid (Skeyword stack s) _ x0) ->
        gotoBinders s (Sbinders stack s (reduction110 x0))
run' S170 stack (TId arg) = shift >> run S113 (Sid stack S170 arg)
run' S170 stack (TKeyword "⇒") = case stack of
    (Sid (Skeyword stack s) _ x0) ->
        gotoBinders s (Sbinders stack s (reduction110 x0))
run' S170 stack (TKeyword "◊") = shift >> run S112 (Skeyword stack S170)
run' S171 stack (TKeyword ".") = case stack of
    (Sbinders (Sid stack s x0) _ x1) ->
        gotoBinders s (Sbinders stack s (reduction109 x0 x1))
run' S171 stack (TKeyword "⇒") = case stack of
    (Sbinders (Sid stack s x0) _ x1) ->
        gotoBinders s (Sbinders stack s (reduction109 x0 x1))
run' S172 stack (TKeyword "export") = case stack of
    (Smonad (Skeyword (Slet stack s x0) _) _ x1) ->
        gotoMonad s (Smonad stack s (reduction32 x0 x1))
run' S172 stack (TKeyword "⟩") = case stack of
    (Smonad (Skeyword (Slet stack s x0) _) _ x1) ->
        gotoMonad s (Smonad stack s (reduction32 x0 x1))
run' S172 stack (TKeyword "def") = case stack of
    (Smonad (Skeyword (Slet stack s x0) _) _ x1) ->
        gotoMonad s (Smonad stack s (reduction32 x0 x1))
run' S172 stack (TKeyword ")") = case stack of
    (Smonad (Skeyword (Slet stack s x0) _) _ x1) ->
        gotoMonad s (Smonad stack s (reduction32 x0 x1))
run' S172 stack (TKeyword "]") = case stack of
    (Smonad (Skeyword (Slet stack s x0) _) _ x1) ->
        gotoMonad s (Smonad stack s (reduction32 x0 x1))
run' S172 stack TEnd = case stack of
    (Smonad (Skeyword (Slet stack s x0) _) _ x1) ->
        gotoMonad s (Smonad stack s (reduction32 x0 x1))
run' S172 stack (TKeyword "import") = case stack of
    (Smonad (Skeyword (Slet stack s x0) _) _ x1) ->
        gotoMonad s (Smonad stack s (reduction32 x0 x1))
run' S172 stack (TKeyword "⟧") = case stack of
    (Smonad (Skeyword (Slet stack s x0) _) _ x1) ->
        gotoMonad s (Smonad stack s (reduction32 x0 x1))
run' S172 stack (TKeyword "|") = case stack of
    (Smonad (Skeyword (Slet stack s x0) _) _ x1) ->
        gotoMonad s (Smonad stack s (reduction32 x0 x1))
run' S172 stack (TKeyword "}") = case stack of
    (Smonad (Skeyword (Slet stack s x0) _) _ x1) ->
        gotoMonad s (Smonad stack s (reduction32 x0 x1))
run' S172 stack (TKeyword "≡") = case stack of
    (Smonad (Skeyword (Slet stack s x0) _) _ x1) ->
        gotoMonad s (Smonad stack s (reduction32 x0 x1))
run' S172 stack (TKeyword ":") = case stack of
    (Smonad (Skeyword (Slet stack s x0) _) _ x1) ->
        gotoMonad s (Smonad stack s (reduction32 x0 x1))
run' S172 stack (TKeyword "⟯") = case stack of
    (Smonad (Skeyword (Slet stack s x0) _) _ x1) ->
        gotoMonad s (Smonad stack s (reduction32 x0 x1))
run' S172 stack (TKeyword "→") = case stack of
    (Smonad (Skeyword (Slet stack s x0) _) _ x1) ->
        gotoMonad s (Smonad stack s (reduction32 x0 x1))
run' S172 stack (TKeyword ",") = case stack of
    (Smonad (Skeyword (Slet stack s x0) _) _ x1) ->
        gotoMonad s (Smonad stack s (reduction32 x0 x1))
run' S173 stack (TKeyword "-") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction83 x0))
run' S173 stack (TKeyword "[") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction83 x0))
run' S173 stack (TKeyword "^") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction83 x0))
run' S173 stack (TKeyword ")") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction83 x0))
run' S173 stack (TKeyword "⟦") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction83 x0))
run' S173 stack (TKeyword "≤") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction83 x0))
run' S173 stack (TKeyword "<") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction83 x0))
run' S173 stack (TKeyword "≠") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction83 x0))
run' S173 stack (TKeyword "{") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction83 x0))
run' S173 stack (TKeyword "⟮") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction83 x0))
run' S173 stack (TReal arg) = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction83 x0))
run' S173 stack (TKeyword "⋆") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction83 x0))
run' S173 stack (TKeyword "*") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction83 x0))
run' S173 stack (TKeyword "def") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction83 x0))
run' S173 stack TEnd = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction83 x0))
run' S173 stack (TInteger arg) = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction83 x0))
run' S173 stack (TKeyword "∘") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction83 x0))
run' S173 stack (TKeyword "⟧") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction83 x0))
run' S173 stack (TKeyword "≥") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction83 x0))
run' S173 stack (TKeyword "|") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction83 x0))
run' S173 stack (TKeyword "=") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction83 x0))
run' S173 stack (TKeyword "≡") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction83 x0))
run' S173 stack (TKeyword "⟯") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction83 x0))
run' S173 stack (TKeyword "→") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction83 x0))
run' S173 stack (TKeyword "+") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction83 x0))
run' S173 stack (TKeyword "⟨") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction83 x0))
run' S173 stack (TKeyword "export") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction83 x0))
run' S173 stack (TId arg) = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction83 x0))
run' S173 stack (TString arg) = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction83 x0))
run' S173 stack (TKeyword "∧") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction83 x0))
run' S173 stack (TKeyword ">") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction83 x0))
run' S173 stack (TKeyword "}") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction83 x0))
run' S173 stack (TKeyword ":") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction83 x0))
run' S173 stack (TKeyword "/") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction83 x0))
run' S173 stack (TKeyword "⊓") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction83 x0))
run' S173 stack (TKeyword "(") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction83 x0))
run' S173 stack (TKeyword "import") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction83 x0))
run' S173 stack (TKeyword "∨") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction83 x0))
run' S173 stack (TKeyword "⟩") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction83 x0))
run' S173 stack (TKeyword "]") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction83 x0))
run' S173 stack (TKeyword ";") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction83 x0))
run' S173 stack (TKeyword "⊔") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction83 x0))
run' S173 stack (TKeyword ",") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoPrim s (Sprim stack s (reduction83 x0))
run' S174 stack (TId arg) = shift >> run S39 (Sid stack S174 arg)
run' S174 stack (TString arg) = shift >> run S36 (Sstring stack S174 arg)
run' S174 stack (TKeyword "{") = shift >> run S42 (Skeyword stack S174)
run' S174 stack (TKeyword "⟮") = shift >> run S43 (Skeyword stack S174)
run' S174 stack (TReal arg) = shift >> run S44 (Sreal stack S174 arg)
run' S174 stack (TKeyword "⋆") = shift >> run S45 (Skeyword stack S174)
run' S174 stack (TKeyword "λ") = shift >> run S46 (Skeyword stack S174)
run' S174 stack (TKeyword "(") = shift >> run S96 (Skeyword stack S174)
run' S174 stack (TKeyword "∃") = shift >> run S52 (Skeyword stack S174)
run' S174 stack (TKeyword "[") = shift >> run S54 (Skeyword stack S174)
run' S174 stack (TInteger arg) = shift >> run S57 (Sinteger stack S174 arg)
run' S174 stack (TKeyword "~") = shift >> run S59 (Skeyword stack S174)
run' S174 stack (TKeyword "¬") = shift >> run S63 (Skeyword stack S174)
run' S175 stack (TKeyword ")") = case stack of
    (Sadd (Skeyword (Smul stack s x0) _) _ x1) ->
        gotoAdd s (Sadd stack s (reduction60 x0 x1))
run' S175 stack (TKeyword "export") = case stack of
    (Sadd (Skeyword (Smul stack s x0) _) _ x1) ->
        gotoAdd s (Sadd stack s (reduction60 x0 x1))
run' S175 stack (TKeyword "import") = case stack of
    (Sadd (Skeyword (Smul stack s x0) _) _ x1) ->
        gotoAdd s (Sadd stack s (reduction60 x0 x1))
run' S175 stack (TKeyword "∧") = case stack of
    (Sadd (Skeyword (Smul stack s x0) _) _ x1) ->
        gotoAdd s (Sadd stack s (reduction60 x0 x1))
run' S175 stack (TKeyword "≤") = case stack of
    (Sadd (Skeyword (Smul stack s x0) _) _ x1) ->
        gotoAdd s (Sadd stack s (reduction60 x0 x1))
run' S175 stack (TKeyword ">") = case stack of
    (Sadd (Skeyword (Smul stack s x0) _) _ x1) ->
        gotoAdd s (Sadd stack s (reduction60 x0 x1))
run' S175 stack (TKeyword "<") = case stack of
    (Sadd (Skeyword (Smul stack s x0) _) _ x1) ->
        gotoAdd s (Sadd stack s (reduction60 x0 x1))
run' S175 stack (TKeyword "}") = case stack of
    (Sadd (Skeyword (Smul stack s x0) _) _ x1) ->
        gotoAdd s (Sadd stack s (reduction60 x0 x1))
run' S175 stack (TKeyword "≠") = case stack of
    (Sadd (Skeyword (Smul stack s x0) _) _ x1) ->
        gotoAdd s (Sadd stack s (reduction60 x0 x1))
run' S175 stack (TKeyword ":") = case stack of
    (Sadd (Skeyword (Smul stack s x0) _) _ x1) ->
        gotoAdd s (Sadd stack s (reduction60 x0 x1))
run' S175 stack (TKeyword "⊓") = case stack of
    (Sadd (Skeyword (Smul stack s x0) _) _ x1) ->
        gotoAdd s (Sadd stack s (reduction60 x0 x1))
run' S175 stack (TKeyword "def") = case stack of
    (Sadd (Skeyword (Smul stack s x0) _) _ x1) ->
        gotoAdd s (Sadd stack s (reduction60 x0 x1))
run' S175 stack (TKeyword "∨") = case stack of
    (Sadd (Skeyword (Smul stack s x0) _) _ x1) ->
        gotoAdd s (Sadd stack s (reduction60 x0 x1))
run' S175 stack (TKeyword "⟩") = case stack of
    (Sadd (Skeyword (Smul stack s x0) _) _ x1) ->
        gotoAdd s (Sadd stack s (reduction60 x0 x1))
run' S175 stack (TKeyword "]") = case stack of
    (Sadd (Skeyword (Smul stack s x0) _) _ x1) ->
        gotoAdd s (Sadd stack s (reduction60 x0 x1))
run' S175 stack TEnd = case stack of
    (Sadd (Skeyword (Smul stack s x0) _) _ x1) ->
        gotoAdd s (Sadd stack s (reduction60 x0 x1))
run' S175 stack (TKeyword "∘") = case stack of
    (Sadd (Skeyword (Smul stack s x0) _) _ x1) ->
        gotoAdd s (Sadd stack s (reduction60 x0 x1))
run' S175 stack (TKeyword "⟧") = case stack of
    (Sadd (Skeyword (Smul stack s x0) _) _ x1) ->
        gotoAdd s (Sadd stack s (reduction60 x0 x1))
run' S175 stack (TKeyword "≥") = case stack of
    (Sadd (Skeyword (Smul stack s x0) _) _ x1) ->
        gotoAdd s (Sadd stack s (reduction60 x0 x1))
run' S175 stack (TKeyword "|") = case stack of
    (Sadd (Skeyword (Smul stack s x0) _) _ x1) ->
        gotoAdd s (Sadd stack s (reduction60 x0 x1))
run' S175 stack (TKeyword "=") = case stack of
    (Sadd (Skeyword (Smul stack s x0) _) _ x1) ->
        gotoAdd s (Sadd stack s (reduction60 x0 x1))
run' S175 stack (TKeyword "≡") = case stack of
    (Sadd (Skeyword (Smul stack s x0) _) _ x1) ->
        gotoAdd s (Sadd stack s (reduction60 x0 x1))
run' S175 stack (TKeyword ";") = case stack of
    (Sadd (Skeyword (Smul stack s x0) _) _ x1) ->
        gotoAdd s (Sadd stack s (reduction60 x0 x1))
run' S175 stack (TKeyword "⊔") = case stack of
    (Sadd (Skeyword (Smul stack s x0) _) _ x1) ->
        gotoAdd s (Sadd stack s (reduction60 x0 x1))
run' S175 stack (TKeyword "⟯") = case stack of
    (Sadd (Skeyword (Smul stack s x0) _) _ x1) ->
        gotoAdd s (Sadd stack s (reduction60 x0 x1))
run' S175 stack (TKeyword "→") = case stack of
    (Sadd (Skeyword (Smul stack s x0) _) _ x1) ->
        gotoAdd s (Sadd stack s (reduction60 x0 x1))
run' S175 stack (TKeyword ",") = case stack of
    (Sadd (Skeyword (Smul stack s x0) _) _ x1) ->
        gotoAdd s (Sadd stack s (reduction60 x0 x1))
run' S176 stack (TKeyword ")") = case stack of
    (Sadd (Skeyword (Smul stack s x0) _) _ x1) ->
        gotoAdd s (Sadd stack s (reduction59 x0 x1))
run' S176 stack (TKeyword "export") = case stack of
    (Sadd (Skeyword (Smul stack s x0) _) _ x1) ->
        gotoAdd s (Sadd stack s (reduction59 x0 x1))
run' S176 stack (TKeyword "import") = case stack of
    (Sadd (Skeyword (Smul stack s x0) _) _ x1) ->
        gotoAdd s (Sadd stack s (reduction59 x0 x1))
run' S176 stack (TKeyword "∧") = case stack of
    (Sadd (Skeyword (Smul stack s x0) _) _ x1) ->
        gotoAdd s (Sadd stack s (reduction59 x0 x1))
run' S176 stack (TKeyword "≤") = case stack of
    (Sadd (Skeyword (Smul stack s x0) _) _ x1) ->
        gotoAdd s (Sadd stack s (reduction59 x0 x1))
run' S176 stack (TKeyword ">") = case stack of
    (Sadd (Skeyword (Smul stack s x0) _) _ x1) ->
        gotoAdd s (Sadd stack s (reduction59 x0 x1))
run' S176 stack (TKeyword "<") = case stack of
    (Sadd (Skeyword (Smul stack s x0) _) _ x1) ->
        gotoAdd s (Sadd stack s (reduction59 x0 x1))
run' S176 stack (TKeyword "}") = case stack of
    (Sadd (Skeyword (Smul stack s x0) _) _ x1) ->
        gotoAdd s (Sadd stack s (reduction59 x0 x1))
run' S176 stack (TKeyword "≠") = case stack of
    (Sadd (Skeyword (Smul stack s x0) _) _ x1) ->
        gotoAdd s (Sadd stack s (reduction59 x0 x1))
run' S176 stack (TKeyword ":") = case stack of
    (Sadd (Skeyword (Smul stack s x0) _) _ x1) ->
        gotoAdd s (Sadd stack s (reduction59 x0 x1))
run' S176 stack (TKeyword "⊓") = case stack of
    (Sadd (Skeyword (Smul stack s x0) _) _ x1) ->
        gotoAdd s (Sadd stack s (reduction59 x0 x1))
run' S176 stack (TKeyword "def") = case stack of
    (Sadd (Skeyword (Smul stack s x0) _) _ x1) ->
        gotoAdd s (Sadd stack s (reduction59 x0 x1))
run' S176 stack (TKeyword "∨") = case stack of
    (Sadd (Skeyword (Smul stack s x0) _) _ x1) ->
        gotoAdd s (Sadd stack s (reduction59 x0 x1))
run' S176 stack (TKeyword "⟩") = case stack of
    (Sadd (Skeyword (Smul stack s x0) _) _ x1) ->
        gotoAdd s (Sadd stack s (reduction59 x0 x1))
run' S176 stack (TKeyword "]") = case stack of
    (Sadd (Skeyword (Smul stack s x0) _) _ x1) ->
        gotoAdd s (Sadd stack s (reduction59 x0 x1))
run' S176 stack TEnd = case stack of
    (Sadd (Skeyword (Smul stack s x0) _) _ x1) ->
        gotoAdd s (Sadd stack s (reduction59 x0 x1))
run' S176 stack (TKeyword "∘") = case stack of
    (Sadd (Skeyword (Smul stack s x0) _) _ x1) ->
        gotoAdd s (Sadd stack s (reduction59 x0 x1))
run' S176 stack (TKeyword "⟧") = case stack of
    (Sadd (Skeyword (Smul stack s x0) _) _ x1) ->
        gotoAdd s (Sadd stack s (reduction59 x0 x1))
run' S176 stack (TKeyword "≥") = case stack of
    (Sadd (Skeyword (Smul stack s x0) _) _ x1) ->
        gotoAdd s (Sadd stack s (reduction59 x0 x1))
run' S176 stack (TKeyword "|") = case stack of
    (Sadd (Skeyword (Smul stack s x0) _) _ x1) ->
        gotoAdd s (Sadd stack s (reduction59 x0 x1))
run' S176 stack (TKeyword "=") = case stack of
    (Sadd (Skeyword (Smul stack s x0) _) _ x1) ->
        gotoAdd s (Sadd stack s (reduction59 x0 x1))
run' S176 stack (TKeyword "≡") = case stack of
    (Sadd (Skeyword (Smul stack s x0) _) _ x1) ->
        gotoAdd s (Sadd stack s (reduction59 x0 x1))
run' S176 stack (TKeyword ";") = case stack of
    (Sadd (Skeyword (Smul stack s x0) _) _ x1) ->
        gotoAdd s (Sadd stack s (reduction59 x0 x1))
run' S176 stack (TKeyword "⊔") = case stack of
    (Sadd (Skeyword (Smul stack s x0) _) _ x1) ->
        gotoAdd s (Sadd stack s (reduction59 x0 x1))
run' S176 stack (TKeyword "⟯") = case stack of
    (Sadd (Skeyword (Smul stack s x0) _) _ x1) ->
        gotoAdd s (Sadd stack s (reduction59 x0 x1))
run' S176 stack (TKeyword "→") = case stack of
    (Sadd (Skeyword (Smul stack s x0) _) _ x1) ->
        gotoAdd s (Sadd stack s (reduction59 x0 x1))
run' S176 stack (TKeyword ",") = case stack of
    (Sadd (Skeyword (Smul stack s x0) _) _ x1) ->
        gotoAdd s (Sadd stack s (reduction59 x0 x1))
run' S177 stack (TKeyword "import") = case stack of
    (Sconj (Skeyword (Snot stack s x0) _) _ x1) ->
        gotoConj s (Sconj stack s (reduction46 x0 x1))
run' S177 stack (TKeyword "⟧") = case stack of
    (Sconj (Skeyword (Snot stack s x0) _) _ x1) ->
        gotoConj s (Sconj stack s (reduction46 x0 x1))
run' S177 stack (TKeyword "∨") = case stack of
    (Sconj (Skeyword (Snot stack s x0) _) _ x1) ->
        gotoConj s (Sconj stack s (reduction46 x0 x1))
run' S177 stack (TKeyword ";") = case stack of
    (Sconj (Skeyword (Snot stack s x0) _) _ x1) ->
        gotoConj s (Sconj stack s (reduction46 x0 x1))
run' S177 stack (TKeyword "⟩") = case stack of
    (Sconj (Skeyword (Snot stack s x0) _) _ x1) ->
        gotoConj s (Sconj stack s (reduction46 x0 x1))
run' S177 stack (TKeyword "def") = case stack of
    (Sconj (Skeyword (Snot stack s x0) _) _ x1) ->
        gotoConj s (Sconj stack s (reduction46 x0 x1))
run' S177 stack (TKeyword "export") = case stack of
    (Sconj (Skeyword (Snot stack s x0) _) _ x1) ->
        gotoConj s (Sconj stack s (reduction46 x0 x1))
run' S177 stack (TKeyword ")") = case stack of
    (Sconj (Skeyword (Snot stack s x0) _) _ x1) ->
        gotoConj s (Sconj stack s (reduction46 x0 x1))
run' S177 stack (TKeyword "|") = case stack of
    (Sconj (Skeyword (Snot stack s x0) _) _ x1) ->
        gotoConj s (Sconj stack s (reduction46 x0 x1))
run' S177 stack (TKeyword "]") = case stack of
    (Sconj (Skeyword (Snot stack s x0) _) _ x1) ->
        gotoConj s (Sconj stack s (reduction46 x0 x1))
run' S177 stack (TKeyword "}") = case stack of
    (Sconj (Skeyword (Snot stack s x0) _) _ x1) ->
        gotoConj s (Sconj stack s (reduction46 x0 x1))
run' S177 stack (TKeyword "≡") = case stack of
    (Sconj (Skeyword (Snot stack s x0) _) _ x1) ->
        gotoConj s (Sconj stack s (reduction46 x0 x1))
run' S177 stack (TKeyword ":") = case stack of
    (Sconj (Skeyword (Snot stack s x0) _) _ x1) ->
        gotoConj s (Sconj stack s (reduction46 x0 x1))
run' S177 stack TEnd = case stack of
    (Sconj (Skeyword (Snot stack s x0) _) _ x1) ->
        gotoConj s (Sconj stack s (reduction46 x0 x1))
run' S177 stack (TKeyword "⊔") = case stack of
    (Sconj (Skeyword (Snot stack s x0) _) _ x1) ->
        gotoConj s (Sconj stack s (reduction46 x0 x1))
run' S177 stack (TKeyword "⟯") = case stack of
    (Sconj (Skeyword (Snot stack s x0) _) _ x1) ->
        gotoConj s (Sconj stack s (reduction46 x0 x1))
run' S177 stack (TKeyword "→") = case stack of
    (Sconj (Skeyword (Snot stack s x0) _) _ x1) ->
        gotoConj s (Sconj stack s (reduction46 x0 x1))
run' S177 stack (TKeyword "⊓") = case stack of
    (Sconj (Skeyword (Snot stack s x0) _) _ x1) ->
        gotoConj s (Sconj stack s (reduction46 x0 x1))
run' S177 stack (TKeyword ",") = case stack of
    (Sconj (Skeyword (Snot stack s x0) _) _ x1) ->
        gotoConj s (Sconj stack s (reduction46 x0 x1))
run' S178 stack TEnd = case stack of
    (Sterm (Skeyword (Sterm stack s x0) _) _ x1) ->
        gotoNormal s (Snormal stack s (reduction21 x0 x1))
run' S178 stack (TKeyword ")") = case stack of
    (Sterm (Skeyword (Sterm stack s x0) _) _ x1) ->
        gotoNormal s (Snormal stack s (reduction21 x0 x1))
run' S178 stack (TKeyword "def") = case stack of
    (Sterm (Skeyword (Sterm stack s x0) _) _ x1) ->
        gotoNormal s (Snormal stack s (reduction21 x0 x1))
run' S178 stack (TKeyword "⟩") = case stack of
    (Sterm (Skeyword (Sterm stack s x0) _) _ x1) ->
        gotoNormal s (Snormal stack s (reduction21 x0 x1))
run' S178 stack (TKeyword "export") = case stack of
    (Sterm (Skeyword (Sterm stack s x0) _) _ x1) ->
        gotoNormal s (Snormal stack s (reduction21 x0 x1))
run' S178 stack (TKeyword "import") = case stack of
    (Sterm (Skeyword (Sterm stack s x0) _) _ x1) ->
        gotoNormal s (Snormal stack s (reduction21 x0 x1))
run' S178 stack (TKeyword "⟧") = case stack of
    (Sterm (Skeyword (Sterm stack s x0) _) _ x1) ->
        gotoNormal s (Snormal stack s (reduction21 x0 x1))
run' S178 stack (TKeyword "|") = case stack of
    (Sterm (Skeyword (Sterm stack s x0) _) _ x1) ->
        gotoNormal s (Snormal stack s (reduction21 x0 x1))
run' S178 stack (TKeyword "}") = case stack of
    (Sterm (Skeyword (Sterm stack s x0) _) _ x1) ->
        gotoNormal s (Snormal stack s (reduction21 x0 x1))
run' S178 stack (TKeyword "⟯") = case stack of
    (Sterm (Skeyword (Sterm stack s x0) _) _ x1) ->
        gotoNormal s (Snormal stack s (reduction21 x0 x1))
run' S178 stack (TKeyword ",") = case stack of
    (Sterm (Skeyword (Sterm stack s x0) _) _ x1) ->
        gotoNormal s (Snormal stack s (reduction21 x0 x1))
run' S179 stack TEnd = case stack of
    (Snormal (Skeyword (Sterm stack s x0) _) _ x1) ->
        gotoNormal s (Snormal stack s (reduction18 x0 x1))
run' S179 stack (TKeyword ")") = case stack of
    (Snormal (Skeyword (Sterm stack s x0) _) _ x1) ->
        gotoNormal s (Snormal stack s (reduction18 x0 x1))
run' S179 stack (TKeyword "def") = case stack of
    (Snormal (Skeyword (Sterm stack s x0) _) _ x1) ->
        gotoNormal s (Snormal stack s (reduction18 x0 x1))
run' S179 stack (TKeyword "⟩") = case stack of
    (Snormal (Skeyword (Sterm stack s x0) _) _ x1) ->
        gotoNormal s (Snormal stack s (reduction18 x0 x1))
run' S179 stack (TKeyword "export") = case stack of
    (Snormal (Skeyword (Sterm stack s x0) _) _ x1) ->
        gotoNormal s (Snormal stack s (reduction18 x0 x1))
run' S179 stack (TKeyword "import") = case stack of
    (Snormal (Skeyword (Sterm stack s x0) _) _ x1) ->
        gotoNormal s (Snormal stack s (reduction18 x0 x1))
run' S179 stack (TKeyword "⟧") = case stack of
    (Snormal (Skeyword (Sterm stack s x0) _) _ x1) ->
        gotoNormal s (Snormal stack s (reduction18 x0 x1))
run' S179 stack (TKeyword "|") = case stack of
    (Snormal (Skeyword (Sterm stack s x0) _) _ x1) ->
        gotoNormal s (Snormal stack s (reduction18 x0 x1))
run' S179 stack (TKeyword "}") = case stack of
    (Snormal (Skeyword (Sterm stack s x0) _) _ x1) ->
        gotoNormal s (Snormal stack s (reduction18 x0 x1))
run' S179 stack (TKeyword "⟯") = case stack of
    (Snormal (Skeyword (Sterm stack s x0) _) _ x1) ->
        gotoNormal s (Snormal stack s (reduction18 x0 x1))
run' S179 stack (TKeyword ",") = case stack of
    (Snormal (Skeyword (Sterm stack s x0) _) _ x1) ->
        gotoNormal s (Snormal stack s (reduction18 x0 x1))
run' S180 stack TEnd = case stack of
    (Snormal (Skeyword (Sterm stack s x0) _) _ x1) ->
        gotoNormal s (Snormal stack s (reduction23 x0 x1))
run' S180 stack (TKeyword ")") = case stack of
    (Snormal (Skeyword (Sterm stack s x0) _) _ x1) ->
        gotoNormal s (Snormal stack s (reduction23 x0 x1))
run' S180 stack (TKeyword "def") = case stack of
    (Snormal (Skeyword (Sterm stack s x0) _) _ x1) ->
        gotoNormal s (Snormal stack s (reduction23 x0 x1))
run' S180 stack (TKeyword "⟩") = case stack of
    (Snormal (Skeyword (Sterm stack s x0) _) _ x1) ->
        gotoNormal s (Snormal stack s (reduction23 x0 x1))
run' S180 stack (TKeyword "export") = case stack of
    (Snormal (Skeyword (Sterm stack s x0) _) _ x1) ->
        gotoNormal s (Snormal stack s (reduction23 x0 x1))
run' S180 stack (TKeyword "import") = case stack of
    (Snormal (Skeyword (Sterm stack s x0) _) _ x1) ->
        gotoNormal s (Snormal stack s (reduction23 x0 x1))
run' S180 stack (TKeyword "⟧") = case stack of
    (Snormal (Skeyword (Sterm stack s x0) _) _ x1) ->
        gotoNormal s (Snormal stack s (reduction23 x0 x1))
run' S180 stack (TKeyword "|") = case stack of
    (Snormal (Skeyword (Sterm stack s x0) _) _ x1) ->
        gotoNormal s (Snormal stack s (reduction23 x0 x1))
run' S180 stack (TKeyword "}") = case stack of
    (Snormal (Skeyword (Sterm stack s x0) _) _ x1) ->
        gotoNormal s (Snormal stack s (reduction23 x0 x1))
run' S180 stack (TKeyword "⟯") = case stack of
    (Snormal (Skeyword (Sterm stack s x0) _) _ x1) ->
        gotoNormal s (Snormal stack s (reduction23 x0 x1))
run' S180 stack (TKeyword ",") = case stack of
    (Snormal (Skeyword (Sterm stack s x0) _) _ x1) ->
        gotoNormal s (Snormal stack s (reduction23 x0 x1))
run' S181 stack (TKeyword "import") = case stack of
    (Sdisj (Skeyword (Sconj2 stack s x0) _) _ x1) ->
        gotoDisj s (Sdisj stack s (reduction40 x0 x1))
run' S181 stack (TKeyword "⟧") = case stack of
    (Sdisj (Skeyword (Sconj2 stack s x0) _) _ x1) ->
        gotoDisj s (Sdisj stack s (reduction40 x0 x1))
run' S181 stack (TKeyword ";") = case stack of
    (Sdisj (Skeyword (Sconj2 stack s x0) _) _ x1) ->
        gotoDisj s (Sdisj stack s (reduction40 x0 x1))
run' S181 stack (TKeyword "⟩") = case stack of
    (Sdisj (Skeyword (Sconj2 stack s x0) _) _ x1) ->
        gotoDisj s (Sdisj stack s (reduction40 x0 x1))
run' S181 stack (TKeyword "def") = case stack of
    (Sdisj (Skeyword (Sconj2 stack s x0) _) _ x1) ->
        gotoDisj s (Sdisj stack s (reduction40 x0 x1))
run' S181 stack (TKeyword "export") = case stack of
    (Sdisj (Skeyword (Sconj2 stack s x0) _) _ x1) ->
        gotoDisj s (Sdisj stack s (reduction40 x0 x1))
run' S181 stack (TKeyword ")") = case stack of
    (Sdisj (Skeyword (Sconj2 stack s x0) _) _ x1) ->
        gotoDisj s (Sdisj stack s (reduction40 x0 x1))
run' S181 stack (TKeyword "|") = case stack of
    (Sdisj (Skeyword (Sconj2 stack s x0) _) _ x1) ->
        gotoDisj s (Sdisj stack s (reduction40 x0 x1))
run' S181 stack (TKeyword "]") = case stack of
    (Sdisj (Skeyword (Sconj2 stack s x0) _) _ x1) ->
        gotoDisj s (Sdisj stack s (reduction40 x0 x1))
run' S181 stack (TKeyword "}") = case stack of
    (Sdisj (Skeyword (Sconj2 stack s x0) _) _ x1) ->
        gotoDisj s (Sdisj stack s (reduction40 x0 x1))
run' S181 stack (TKeyword "≡") = case stack of
    (Sdisj (Skeyword (Sconj2 stack s x0) _) _ x1) ->
        gotoDisj s (Sdisj stack s (reduction40 x0 x1))
run' S181 stack (TKeyword ":") = case stack of
    (Sdisj (Skeyword (Sconj2 stack s x0) _) _ x1) ->
        gotoDisj s (Sdisj stack s (reduction40 x0 x1))
run' S181 stack TEnd = case stack of
    (Sdisj (Skeyword (Sconj2 stack s x0) _) _ x1) ->
        gotoDisj s (Sdisj stack s (reduction40 x0 x1))
run' S181 stack (TKeyword "⟯") = case stack of
    (Sdisj (Skeyword (Sconj2 stack s x0) _) _ x1) ->
        gotoDisj s (Sdisj stack s (reduction40 x0 x1))
run' S181 stack (TKeyword "→") = case stack of
    (Sdisj (Skeyword (Sconj2 stack s x0) _) _ x1) ->
        gotoDisj s (Sdisj stack s (reduction40 x0 x1))
run' S181 stack (TKeyword ",") = case stack of
    (Sdisj (Skeyword (Sconj2 stack s x0) _) _ x1) ->
        gotoDisj s (Sdisj stack s (reduction40 x0 x1))
run' S182 stack (TKeyword "⟩") = case stack of
    (Slabtypes (Skeyword (Slabtype stack s x0) _) _ x1) ->
        gotoLabtypes s (Slabtypes stack s (reduction98 x0 x1))
run' S183 stack (TKeyword "⟩") = case stack of
    (Snormal (Skeyword (Sid stack s x0) _) _ x1) ->
        gotoLabtype s (Slabtype stack s (reduction99 x0 x1))
run' S183 stack (TKeyword ",") = case stack of
    (Snormal (Skeyword (Sid stack s x0) _) _ x1) ->
        gotoLabtype s (Slabtype stack s (reduction99 x0 x1))
run' S184 stack (TKeyword "≡") = shift >> run S205 (Skeyword stack S184)
run' S185 stack (TKeyword "-") = shift >> run S28 (Skeyword stack S185)
run' S185 stack (TKeyword "⋆") = shift >> run S45 (Skeyword stack S185)
run' S185 stack (TKeyword "⟨") = shift >> run S31 (Skeyword stack S185)
run' S185 stack (TKeyword "λ") = shift >> run S46 (Skeyword stack S185)
run' S185 stack (TKeyword "(") = shift >> run S49 (Skeyword stack S185)
run' S185 stack (TKeyword "∃") = shift >> run S52 (Skeyword stack S185)
run' S185 stack (TKeyword "[") = shift >> run S54 (Skeyword stack S185)
run' S185 stack (TInteger arg) = shift >> run S57 (Sinteger stack S185 arg)
run' S185 stack (TString arg) = shift >> run S36 (Sstring stack S185 arg)
run' S185 stack (TKeyword "⟦") = shift >> run S37 (Skeyword stack S185)
run' S185 stack (TKeyword "~") = shift >> run S59 (Skeyword stack S185)
run' S185 stack (TId arg) = shift >> run S39 (Sid stack S185 arg)
run' S185 stack (TKeyword "{") = shift >> run S42 (Skeyword stack S185)
run' S185 stack (TReal arg) = shift >> run S44 (Sreal stack S185 arg)
run' S185 stack (TKeyword "⟮") = shift >> run S43 (Skeyword stack S185)
run' S185 stack (TKeyword "¬") = shift >> run S63 (Skeyword stack S185)
run' S186 stack TEnd = case stack of
    (Snormal (Skeyword (Sterm (Sstructdef stack s x0) _ x1) _) _ x2) ->
        gotoNormal s (Snormal stack s (reduction17 x0 x1 x2))
run' S186 stack (TKeyword ")") = case stack of
    (Snormal (Skeyword (Sterm (Sstructdef stack s x0) _ x1) _) _ x2) ->
        gotoNormal s (Snormal stack s (reduction17 x0 x1 x2))
run' S186 stack (TKeyword "def") = case stack of
    (Snormal (Skeyword (Sterm (Sstructdef stack s x0) _ x1) _) _ x2) ->
        gotoNormal s (Snormal stack s (reduction17 x0 x1 x2))
run' S186 stack (TKeyword "⟩") = case stack of
    (Snormal (Skeyword (Sterm (Sstructdef stack s x0) _ x1) _) _ x2) ->
        gotoNormal s (Snormal stack s (reduction17 x0 x1 x2))
run' S186 stack (TKeyword "export") = case stack of
    (Snormal (Skeyword (Sterm (Sstructdef stack s x0) _ x1) _) _ x2) ->
        gotoNormal s (Snormal stack s (reduction17 x0 x1 x2))
run' S186 stack (TKeyword "import") = case stack of
    (Snormal (Skeyword (Sterm (Sstructdef stack s x0) _ x1) _) _ x2) ->
        gotoNormal s (Snormal stack s (reduction17 x0 x1 x2))
run' S186 stack (TKeyword "⟧") = case stack of
    (Snormal (Skeyword (Sterm (Sstructdef stack s x0) _ x1) _) _ x2) ->
        gotoNormal s (Snormal stack s (reduction17 x0 x1 x2))
run' S186 stack (TKeyword "|") = case stack of
    (Snormal (Skeyword (Sterm (Sstructdef stack s x0) _ x1) _) _ x2) ->
        gotoNormal s (Snormal stack s (reduction17 x0 x1 x2))
run' S186 stack (TKeyword "}") = case stack of
    (Snormal (Skeyword (Sterm (Sstructdef stack s x0) _ x1) _) _ x2) ->
        gotoNormal s (Snormal stack s (reduction17 x0 x1 x2))
run' S186 stack (TKeyword "⟯") = case stack of
    (Snormal (Skeyword (Sterm (Sstructdef stack s x0) _ x1) _) _ x2) ->
        gotoNormal s (Snormal stack s (reduction17 x0 x1 x2))
run' S186 stack (TKeyword ",") = case stack of
    (Snormal (Skeyword (Sterm (Sstructdef stack s x0) _ x1) _) _ x2) ->
        gotoNormal s (Snormal stack s (reduction17 x0 x1 x2))
run' S187 stack (TId arg) = shift >> run S39 (Sid stack S187 arg)
run' S187 stack (TKeyword "{") = shift >> run S42 (Skeyword stack S187)
run' S187 stack (TReal arg) = shift >> run S44 (Sreal stack S187 arg)
run' S187 stack (TKeyword "⋆") = shift >> run S45 (Skeyword stack S187)
run' S187 stack (TString arg) = shift >> run S36 (Sstring stack S187 arg)
run' S187 stack (TKeyword "(") = shift >> run S96 (Skeyword stack S187)
run' S187 stack (TKeyword "[") = shift >> run S54 (Skeyword stack S187)
run' S187 stack (TKeyword "⟮") = shift >> run S43 (Skeyword stack S187)
run' S187 stack (TInteger arg) = shift >> run S57 (Sinteger stack S187 arg)
run' S187 stack (TKeyword "~") = shift >> run S59 (Skeyword stack S187)
run' S187 stack (TKeyword "¬") = shift >> run S63 (Skeyword stack S187)
run' S188 stack (TId arg) = shift >> run S208 (Sid stack S188 arg)
run' S188 stack (TKeyword "{") = shift >> run S42 (Skeyword stack S188)
run' S188 stack (TReal arg) = shift >> run S44 (Sreal stack S188 arg)
run' S188 stack (TKeyword "⋆") = shift >> run S45 (Skeyword stack S188)
run' S188 stack (TString arg) = shift >> run S36 (Sstring stack S188 arg)
run' S188 stack (TKeyword "(") = shift >> run S96 (Skeyword stack S188)
run' S188 stack (TKeyword "[") = shift >> run S54 (Skeyword stack S188)
run' S188 stack (TKeyword "⟮") = shift >> run S43 (Skeyword stack S188)
run' S188 stack (TInteger arg) = shift >> run S57 (Sinteger stack S188 arg)
run' S188 stack (TKeyword "~") = shift >> run S59 (Skeyword stack S188)
run' S188 stack (TKeyword "¬") = shift >> run S63 (Skeyword stack S188)
run' S189 stack (TKeyword "⟮") = shift >> run S210 (Skeyword stack S189)
run' S190 stack (TReal arg) = case stack of
    (Skeyword (Slabterms (Skeyword stack s) _ x0) _) ->
        gotoStruct s (Sstruct stack s (reduction87 x0))
run' S190 stack (TKeyword "⋆") = case stack of
    (Skeyword (Slabterms (Skeyword stack s) _ x0) _) ->
        gotoStruct s (Sstruct stack s (reduction87 x0))
run' S190 stack (TKeyword "{") = case stack of
    (Skeyword (Slabterms (Skeyword stack s) _ x0) _) ->
        gotoStruct s (Sstruct stack s (reduction87 x0))
run' S190 stack (TKeyword "[") = case stack of
    (Skeyword (Slabterms (Skeyword stack s) _ x0) _) ->
        gotoStruct s (Sstruct stack s (reduction87 x0))
run' S190 stack (TInteger arg) = case stack of
    (Skeyword (Slabterms (Skeyword stack s) _ x0) _) ->
        gotoStruct s (Sstruct stack s (reduction87 x0))
run' S190 stack (TKeyword "(") = case stack of
    (Skeyword (Slabterms (Skeyword stack s) _ x0) _) ->
        gotoStruct s (Sstruct stack s (reduction87 x0))
run' S190 stack (TId arg) = case stack of
    (Skeyword (Slabterms (Skeyword stack s) _ x0) _) ->
        gotoStruct s (Sstruct stack s (reduction87 x0))
run' S190 stack (TString arg) = case stack of
    (Skeyword (Slabterms (Skeyword stack s) _ x0) _) ->
        gotoStruct s (Sstruct stack s (reduction87 x0))
run' S191 stack (TReal arg) = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoStruct s (Sstruct stack s (reduction88 x0))
run' S191 stack (TKeyword "⋆") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoStruct s (Sstruct stack s (reduction88 x0))
run' S191 stack (TKeyword "{") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoStruct s (Sstruct stack s (reduction88 x0))
run' S191 stack (TKeyword "[") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoStruct s (Sstruct stack s (reduction88 x0))
run' S191 stack (TInteger arg) = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoStruct s (Sstruct stack s (reduction88 x0))
run' S191 stack (TKeyword "(") = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoStruct s (Sstruct stack s (reduction88 x0))
run' S191 stack (TId arg) = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoStruct s (Sstruct stack s (reduction88 x0))
run' S191 stack (TString arg) = case stack of
    (Skeyword (Sterms (Skeyword stack s) _ x0) _) ->
        gotoStruct s (Sstruct stack s (reduction88 x0))
run' S192 stack (TKeyword "-") = case stack of
    (Skeyword (Scases (Skeyword (Scall stack s x0) _) _ x1) _) ->
        gotoCall s (Scall stack s (reduction70 x0 x1))
run' S192 stack (TKeyword "[") = case stack of
    (Skeyword (Scases (Skeyword (Scall stack s x0) _) _ x1) _) ->
        gotoCall s (Scall stack s (reduction70 x0 x1))
run' S192 stack (TKeyword "^") = case stack of
    (Skeyword (Scases (Skeyword (Scall stack s x0) _) _ x1) _) ->
        gotoCall s (Scall stack s (reduction70 x0 x1))
run' S192 stack (TKeyword ")") = case stack of
    (Skeyword (Scases (Skeyword (Scall stack s x0) _) _ x1) _) ->
        gotoCall s (Scall stack s (reduction70 x0 x1))
run' S192 stack (TKeyword "⟦") = case stack of
    (Skeyword (Scases (Skeyword (Scall stack s x0) _) _ x1) _) ->
        gotoCall s (Scall stack s (reduction70 x0 x1))
run' S192 stack (TKeyword "≤") = case stack of
    (Skeyword (Scases (Skeyword (Scall stack s x0) _) _ x1) _) ->
        gotoCall s (Scall stack s (reduction70 x0 x1))
run' S192 stack (TKeyword "<") = case stack of
    (Skeyword (Scases (Skeyword (Scall stack s x0) _) _ x1) _) ->
        gotoCall s (Scall stack s (reduction70 x0 x1))
run' S192 stack (TKeyword "≠") = case stack of
    (Skeyword (Scases (Skeyword (Scall stack s x0) _) _ x1) _) ->
        gotoCall s (Scall stack s (reduction70 x0 x1))
run' S192 stack (TKeyword "{") = case stack of
    (Skeyword (Scases (Skeyword (Scall stack s x0) _) _ x1) _) ->
        gotoCall s (Scall stack s (reduction70 x0 x1))
run' S192 stack (TKeyword "⟮") = case stack of
    (Skeyword (Scases (Skeyword (Scall stack s x0) _) _ x1) _) ->
        gotoCall s (Scall stack s (reduction70 x0 x1))
run' S192 stack (TReal arg) = case stack of
    (Skeyword (Scases (Skeyword (Scall stack s x0) _) _ x1) _) ->
        gotoCall s (Scall stack s (reduction70 x0 x1))
run' S192 stack (TKeyword "⋆") = case stack of
    (Skeyword (Scases (Skeyword (Scall stack s x0) _) _ x1) _) ->
        gotoCall s (Scall stack s (reduction70 x0 x1))
run' S192 stack (TKeyword "*") = case stack of
    (Skeyword (Scases (Skeyword (Scall stack s x0) _) _ x1) _) ->
        gotoCall s (Scall stack s (reduction70 x0 x1))
run' S192 stack (TKeyword "def") = case stack of
    (Skeyword (Scases (Skeyword (Scall stack s x0) _) _ x1) _) ->
        gotoCall s (Scall stack s (reduction70 x0 x1))
run' S192 stack TEnd = case stack of
    (Skeyword (Scases (Skeyword (Scall stack s x0) _) _ x1) _) ->
        gotoCall s (Scall stack s (reduction70 x0 x1))
run' S192 stack (TInteger arg) = case stack of
    (Skeyword (Scases (Skeyword (Scall stack s x0) _) _ x1) _) ->
        gotoCall s (Scall stack s (reduction70 x0 x1))
run' S192 stack (TKeyword "∘") = case stack of
    (Skeyword (Scases (Skeyword (Scall stack s x0) _) _ x1) _) ->
        gotoCall s (Scall stack s (reduction70 x0 x1))
run' S192 stack (TKeyword "⟧") = case stack of
    (Skeyword (Scases (Skeyword (Scall stack s x0) _) _ x1) _) ->
        gotoCall s (Scall stack s (reduction70 x0 x1))
run' S192 stack (TKeyword "≥") = case stack of
    (Skeyword (Scases (Skeyword (Scall stack s x0) _) _ x1) _) ->
        gotoCall s (Scall stack s (reduction70 x0 x1))
run' S192 stack (TKeyword "|") = case stack of
    (Skeyword (Scases (Skeyword (Scall stack s x0) _) _ x1) _) ->
        gotoCall s (Scall stack s (reduction70 x0 x1))
run' S192 stack (TKeyword "=") = case stack of
    (Skeyword (Scases (Skeyword (Scall stack s x0) _) _ x1) _) ->
        gotoCall s (Scall stack s (reduction70 x0 x1))
run' S192 stack (TKeyword "≡") = case stack of
    (Skeyword (Scases (Skeyword (Scall stack s x0) _) _ x1) _) ->
        gotoCall s (Scall stack s (reduction70 x0 x1))
run' S192 stack (TKeyword "⟯") = case stack of
    (Skeyword (Scases (Skeyword (Scall stack s x0) _) _ x1) _) ->
        gotoCall s (Scall stack s (reduction70 x0 x1))
run' S192 stack (TKeyword "→") = case stack of
    (Skeyword (Scases (Skeyword (Scall stack s x0) _) _ x1) _) ->
        gotoCall s (Scall stack s (reduction70 x0 x1))
run' S192 stack (TKeyword "+") = case stack of
    (Skeyword (Scases (Skeyword (Scall stack s x0) _) _ x1) _) ->
        gotoCall s (Scall stack s (reduction70 x0 x1))
run' S192 stack (TKeyword "⟨") = case stack of
    (Skeyword (Scases (Skeyword (Scall stack s x0) _) _ x1) _) ->
        gotoCall s (Scall stack s (reduction70 x0 x1))
run' S192 stack (TKeyword "export") = case stack of
    (Skeyword (Scases (Skeyword (Scall stack s x0) _) _ x1) _) ->
        gotoCall s (Scall stack s (reduction70 x0 x1))
run' S192 stack (TId arg) = case stack of
    (Skeyword (Scases (Skeyword (Scall stack s x0) _) _ x1) _) ->
        gotoCall s (Scall stack s (reduction70 x0 x1))
run' S192 stack (TString arg) = case stack of
    (Skeyword (Scases (Skeyword (Scall stack s x0) _) _ x1) _) ->
        gotoCall s (Scall stack s (reduction70 x0 x1))
run' S192 stack (TKeyword "∧") = case stack of
    (Skeyword (Scases (Skeyword (Scall stack s x0) _) _ x1) _) ->
        gotoCall s (Scall stack s (reduction70 x0 x1))
run' S192 stack (TKeyword ">") = case stack of
    (Skeyword (Scases (Skeyword (Scall stack s x0) _) _ x1) _) ->
        gotoCall s (Scall stack s (reduction70 x0 x1))
run' S192 stack (TKeyword "}") = case stack of
    (Skeyword (Scases (Skeyword (Scall stack s x0) _) _ x1) _) ->
        gotoCall s (Scall stack s (reduction70 x0 x1))
run' S192 stack (TKeyword ":") = case stack of
    (Skeyword (Scases (Skeyword (Scall stack s x0) _) _ x1) _) ->
        gotoCall s (Scall stack s (reduction70 x0 x1))
run' S192 stack (TKeyword "/") = case stack of
    (Skeyword (Scases (Skeyword (Scall stack s x0) _) _ x1) _) ->
        gotoCall s (Scall stack s (reduction70 x0 x1))
run' S192 stack (TKeyword "⊓") = case stack of
    (Skeyword (Scases (Skeyword (Scall stack s x0) _) _ x1) _) ->
        gotoCall s (Scall stack s (reduction70 x0 x1))
run' S192 stack (TKeyword "(") = case stack of
    (Skeyword (Scases (Skeyword (Scall stack s x0) _) _ x1) _) ->
        gotoCall s (Scall stack s (reduction70 x0 x1))
run' S192 stack (TKeyword "import") = case stack of
    (Skeyword (Scases (Skeyword (Scall stack s x0) _) _ x1) _) ->
        gotoCall s (Scall stack s (reduction70 x0 x1))
run' S192 stack (TKeyword "∨") = case stack of
    (Skeyword (Scases (Skeyword (Scall stack s x0) _) _ x1) _) ->
        gotoCall s (Scall stack s (reduction70 x0 x1))
run' S192 stack (TKeyword "⟩") = case stack of
    (Skeyword (Scases (Skeyword (Scall stack s x0) _) _ x1) _) ->
        gotoCall s (Scall stack s (reduction70 x0 x1))
run' S192 stack (TKeyword "]") = case stack of
    (Skeyword (Scases (Skeyword (Scall stack s x0) _) _ x1) _) ->
        gotoCall s (Scall stack s (reduction70 x0 x1))
run' S192 stack (TKeyword ";") = case stack of
    (Skeyword (Scases (Skeyword (Scall stack s x0) _) _ x1) _) ->
        gotoCall s (Scall stack s (reduction70 x0 x1))
run' S192 stack (TKeyword "⊔") = case stack of
    (Skeyword (Scases (Skeyword (Scall stack s x0) _) _ x1) _) ->
        gotoCall s (Scall stack s (reduction70 x0 x1))
run' S192 stack (TKeyword ",") = case stack of
    (Skeyword (Scases (Skeyword (Scall stack s x0) _) _ x1) _) ->
        gotoCall s (Scall stack s (reduction70 x0 x1))
run' S193 stack (TKeyword "}") = shift >> run S211 (Skeyword stack S193)
run' S194 stack (TKeyword "}") = case stack of
    (Snormal (Skeyword (Sid stack s x0) _) _ x1) ->
        gotoLabterm s (Slabterm stack s (reduction96 x0 x1))
run' S194 stack (TKeyword "|") = case stack of
    (Snormal (Skeyword (Sid stack s x0) _) _ x1) ->
        gotoLabterm s (Slabterm stack s (reduction96 x0 x1))
run' S194 stack (TKeyword "⟩") = case stack of
    (Snormal (Skeyword (Sid stack s x0) _) _ x1) ->
        gotoLabterm s (Slabterm stack s (reduction96 x0 x1))
run' S194 stack (TKeyword ",") = case stack of
    (Snormal (Skeyword (Sid stack s x0) _) _ x1) ->
        gotoLabterm s (Slabterm stack s (reduction96 x0 x1))
run' S195 stack (TKeyword "=") = shift >> run S152 (Skeyword stack S195)
run' S196 stack (TKeyword "}") = case stack of
    (Slabterms (Skeyword (Slabterm stack s x0) _) _ x1) ->
        gotoLabterms s (Slabterms stack s (reduction95 x0 x1))
run' S196 stack (TKeyword "⟩") = case stack of
    (Slabterms (Skeyword (Slabterm stack s x0) _) _ x1) ->
        gotoLabterms s (Slabterms stack s (reduction95 x0 x1))
run' S196 stack (TKeyword "|") = case stack of
    (Slabterms (Skeyword (Slabterm stack s x0) _) _ x1) ->
        gotoLabterms s (Slabterms stack s (reduction95 x0 x1))
run' S197 stack (TKeyword "]") = case stack of
    (Sterms (Skeyword (Sterm stack s x0) _) _ x1) ->
        gotoTerms s (Sterms stack s (reduction26 x0 x1))
run' S197 stack (TKeyword "}") = case stack of
    (Sterms (Skeyword (Sterm stack s x0) _) _ x1) ->
        gotoTerms s (Sterms stack s (reduction26 x0 x1))
run' S197 stack (TKeyword "⟩") = case stack of
    (Sterms (Skeyword (Sterm stack s x0) _) _ x1) ->
        gotoTerms s (Sterms stack s (reduction26 x0 x1))
run' S197 stack (TKeyword "|") = case stack of
    (Sterms (Skeyword (Sterm stack s x0) _) _ x1) ->
        gotoTerms s (Sterms stack s (reduction26 x0 x1))
run' S198 stack (TKeyword "⟯") = case stack of
    (Scases (Skeyword (Scase stack s x0) _) _ x1) ->
        gotoCases s (Scases stack s (reduction104 x0 x1))
run' S199 stack (TKeyword "-") = shift >> run S28 (Skeyword stack S199)
run' S199 stack (TKeyword "⋆") = shift >> run S45 (Skeyword stack S199)
run' S199 stack (TKeyword "⟨") = shift >> run S31 (Skeyword stack S199)
run' S199 stack (TKeyword "λ") = shift >> run S46 (Skeyword stack S199)
run' S199 stack (TKeyword "(") = shift >> run S49 (Skeyword stack S199)
run' S199 stack (TKeyword "∃") = shift >> run S52 (Skeyword stack S199)
run' S199 stack (TKeyword "[") = shift >> run S54 (Skeyword stack S199)
run' S199 stack (TInteger arg) = shift >> run S57 (Sinteger stack S199 arg)
run' S199 stack (TString arg) = shift >> run S36 (Sstring stack S199 arg)
run' S199 stack (TKeyword "⟦") = shift >> run S37 (Skeyword stack S199)
run' S199 stack (TKeyword "~") = shift >> run S59 (Skeyword stack S199)
run' S199 stack (TId arg) = shift >> run S39 (Sid stack S199 arg)
run' S199 stack (TKeyword "{") = shift >> run S42 (Skeyword stack S199)
run' S199 stack (TReal arg) = shift >> run S44 (Sreal stack S199 arg)
run' S199 stack (TKeyword "⟮") = shift >> run S43 (Skeyword stack S199)
run' S199 stack (TKeyword "¬") = shift >> run S63 (Skeyword stack S199)
run' S200 stack (TKeyword "export") = case stack of
    (Slambda (Skeyword (Slambdabinders (Skeyword stack s) _ x0) _) _ x1) ->
        gotoLambda s (Slambda stack s (reduction29 x0 x1))
run' S200 stack (TKeyword "⟩") = case stack of
    (Slambda (Skeyword (Slambdabinders (Skeyword stack s) _ x0) _) _ x1) ->
        gotoLambda s (Slambda stack s (reduction29 x0 x1))
run' S200 stack (TKeyword "def") = case stack of
    (Slambda (Skeyword (Slambdabinders (Skeyword stack s) _ x0) _) _ x1) ->
        gotoLambda s (Slambda stack s (reduction29 x0 x1))
run' S200 stack (TKeyword ")") = case stack of
    (Slambda (Skeyword (Slambdabinders (Skeyword stack s) _ x0) _) _ x1) ->
        gotoLambda s (Slambda stack s (reduction29 x0 x1))
run' S200 stack (TKeyword "]") = case stack of
    (Slambda (Skeyword (Slambdabinders (Skeyword stack s) _ x0) _) _ x1) ->
        gotoLambda s (Slambda stack s (reduction29 x0 x1))
run' S200 stack TEnd = case stack of
    (Slambda (Skeyword (Slambdabinders (Skeyword stack s) _ x0) _) _ x1) ->
        gotoLambda s (Slambda stack s (reduction29 x0 x1))
run' S200 stack (TKeyword "import") = case stack of
    (Slambda (Skeyword (Slambdabinders (Skeyword stack s) _ x0) _) _ x1) ->
        gotoLambda s (Slambda stack s (reduction29 x0 x1))
run' S200 stack (TKeyword "⟧") = case stack of
    (Slambda (Skeyword (Slambdabinders (Skeyword stack s) _ x0) _) _ x1) ->
        gotoLambda s (Slambda stack s (reduction29 x0 x1))
run' S200 stack (TKeyword "|") = case stack of
    (Slambda (Skeyword (Slambdabinders (Skeyword stack s) _ x0) _) _ x1) ->
        gotoLambda s (Slambda stack s (reduction29 x0 x1))
run' S200 stack (TKeyword "}") = case stack of
    (Slambda (Skeyword (Slambdabinders (Skeyword stack s) _ x0) _) _ x1) ->
        gotoLambda s (Slambda stack s (reduction29 x0 x1))
run' S200 stack (TKeyword "≡") = case stack of
    (Slambda (Skeyword (Slambdabinders (Skeyword stack s) _ x0) _) _ x1) ->
        gotoLambda s (Slambda stack s (reduction29 x0 x1))
run' S200 stack (TKeyword ":") = case stack of
    (Slambda (Skeyword (Slambdabinders (Skeyword stack s) _ x0) _) _ x1) ->
        gotoLambda s (Slambda stack s (reduction29 x0 x1))
run' S200 stack (TKeyword "⟯") = case stack of
    (Slambda (Skeyword (Slambdabinders (Skeyword stack s) _ x0) _) _ x1) ->
        gotoLambda s (Slambda stack s (reduction29 x0 x1))
run' S200 stack (TKeyword "→") = case stack of
    (Slambda (Skeyword (Slambdabinders (Skeyword stack s) _ x0) _) _ x1) ->
        gotoLambda s (Slambda stack s (reduction29 x0 x1))
run' S200 stack (TKeyword ",") = case stack of
    (Slambda (Skeyword (Slambdabinders (Skeyword stack s) _ x0) _) _ x1) ->
        gotoLambda s (Slambda stack s (reduction29 x0 x1))
run' S201 stack (TKeyword ")") = shift >> run S213 (Skeyword stack S201)
run' S202 stack (TKeyword "export") = case stack of
    (Slambda (Skeyword (Sbinders (Skeyword stack s) _ x0) _) _ x1) ->
        gotoLambda s (Slambda stack s (reduction30 x0 x1))
run' S202 stack (TKeyword "⟩") = case stack of
    (Slambda (Skeyword (Sbinders (Skeyword stack s) _ x0) _) _ x1) ->
        gotoLambda s (Slambda stack s (reduction30 x0 x1))
run' S202 stack (TKeyword "def") = case stack of
    (Slambda (Skeyword (Sbinders (Skeyword stack s) _ x0) _) _ x1) ->
        gotoLambda s (Slambda stack s (reduction30 x0 x1))
run' S202 stack (TKeyword ")") = case stack of
    (Slambda (Skeyword (Sbinders (Skeyword stack s) _ x0) _) _ x1) ->
        gotoLambda s (Slambda stack s (reduction30 x0 x1))
run' S202 stack (TKeyword "]") = case stack of
    (Slambda (Skeyword (Sbinders (Skeyword stack s) _ x0) _) _ x1) ->
        gotoLambda s (Slambda stack s (reduction30 x0 x1))
run' S202 stack TEnd = case stack of
    (Slambda (Skeyword (Sbinders (Skeyword stack s) _ x0) _) _ x1) ->
        gotoLambda s (Slambda stack s (reduction30 x0 x1))
run' S202 stack (TKeyword "import") = case stack of
    (Slambda (Skeyword (Sbinders (Skeyword stack s) _ x0) _) _ x1) ->
        gotoLambda s (Slambda stack s (reduction30 x0 x1))
run' S202 stack (TKeyword "⟧") = case stack of
    (Slambda (Skeyword (Sbinders (Skeyword stack s) _ x0) _) _ x1) ->
        gotoLambda s (Slambda stack s (reduction30 x0 x1))
run' S202 stack (TKeyword "|") = case stack of
    (Slambda (Skeyword (Sbinders (Skeyword stack s) _ x0) _) _ x1) ->
        gotoLambda s (Slambda stack s (reduction30 x0 x1))
run' S202 stack (TKeyword "}") = case stack of
    (Slambda (Skeyword (Sbinders (Skeyword stack s) _ x0) _) _ x1) ->
        gotoLambda s (Slambda stack s (reduction30 x0 x1))
run' S202 stack (TKeyword "≡") = case stack of
    (Slambda (Skeyword (Sbinders (Skeyword stack s) _ x0) _) _ x1) ->
        gotoLambda s (Slambda stack s (reduction30 x0 x1))
run' S202 stack (TKeyword ":") = case stack of
    (Slambda (Skeyword (Sbinders (Skeyword stack s) _ x0) _) _ x1) ->
        gotoLambda s (Slambda stack s (reduction30 x0 x1))
run' S202 stack (TKeyword "⟯") = case stack of
    (Slambda (Skeyword (Sbinders (Skeyword stack s) _ x0) _) _ x1) ->
        gotoLambda s (Slambda stack s (reduction30 x0 x1))
run' S202 stack (TKeyword "→") = case stack of
    (Slambda (Skeyword (Sbinders (Skeyword stack s) _ x0) _) _ x1) ->
        gotoLambda s (Slambda stack s (reduction30 x0 x1))
run' S202 stack (TKeyword ",") = case stack of
    (Slambda (Skeyword (Sbinders (Skeyword stack s) _ x0) _) _ x1) ->
        gotoLambda s (Slambda stack s (reduction30 x0 x1))
run' S203 stack (TKeyword ".") = case stack of
    (Sbinders (Sid (Skeyword stack s) _ x0) _ x1) ->
        gotoBinders s (Sbinders stack s (reduction111 x0 x1))
run' S203 stack (TKeyword "⇒") = case stack of
    (Sbinders (Sid (Skeyword stack s) _ x0) _ x1) ->
        gotoBinders s (Sbinders stack s (reduction111 x0 x1))
run' S204 stack (TKeyword "]") = shift >> run S214 (Skeyword stack S204)
run' S205 stack (TId arg) = shift >> run S39 (Sid stack S205 arg)
run' S205 stack (TString arg) = shift >> run S36 (Sstring stack S205 arg)
run' S205 stack (TKeyword "{") = shift >> run S42 (Skeyword stack S205)
run' S205 stack (TKeyword "⟮") = shift >> run S43 (Skeyword stack S205)
run' S205 stack (TReal arg) = shift >> run S44 (Sreal stack S205 arg)
run' S205 stack (TKeyword "⋆") = shift >> run S45 (Skeyword stack S205)
run' S205 stack (TKeyword "λ") = shift >> run S46 (Skeyword stack S205)
run' S205 stack (TKeyword "(") = shift >> run S96 (Skeyword stack S205)
run' S205 stack (TKeyword "∃") = shift >> run S52 (Skeyword stack S205)
run' S205 stack (TKeyword "[") = shift >> run S54 (Skeyword stack S205)
run' S205 stack (TInteger arg) = shift >> run S57 (Sinteger stack S205 arg)
run' S205 stack (TKeyword "~") = shift >> run S59 (Skeyword stack S205)
run' S205 stack (TKeyword "¬") = shift >> run S63 (Skeyword stack S205)
run' S206 stack (TKeyword ")") = shift >> run S216 (Skeyword stack S206)
run' S207 stack (TKeyword "export") = case stack of
    (Smonad (Skeyword (Scocase (Skeyword (Sid stack s x0) _) _ x1) _) _ x2) ->
        gotoMonad s (Smonad stack s (reduction33 x0 x1 x2))
run' S207 stack (TKeyword "⟩") = case stack of
    (Smonad (Skeyword (Scocase (Skeyword (Sid stack s x0) _) _ x1) _) _ x2) ->
        gotoMonad s (Smonad stack s (reduction33 x0 x1 x2))
run' S207 stack (TKeyword "def") = case stack of
    (Smonad (Skeyword (Scocase (Skeyword (Sid stack s x0) _) _ x1) _) _ x2) ->
        gotoMonad s (Smonad stack s (reduction33 x0 x1 x2))
run' S207 stack (TKeyword ")") = case stack of
    (Smonad (Skeyword (Scocase (Skeyword (Sid stack s x0) _) _ x1) _) _ x2) ->
        gotoMonad s (Smonad stack s (reduction33 x0 x1 x2))
run' S207 stack (TKeyword "]") = case stack of
    (Smonad (Skeyword (Scocase (Skeyword (Sid stack s x0) _) _ x1) _) _ x2) ->
        gotoMonad s (Smonad stack s (reduction33 x0 x1 x2))
run' S207 stack TEnd = case stack of
    (Smonad (Skeyword (Scocase (Skeyword (Sid stack s x0) _) _ x1) _) _ x2) ->
        gotoMonad s (Smonad stack s (reduction33 x0 x1 x2))
run' S207 stack (TKeyword "import") = case stack of
    (Smonad (Skeyword (Scocase (Skeyword (Sid stack s x0) _) _ x1) _) _ x2) ->
        gotoMonad s (Smonad stack s (reduction33 x0 x1 x2))
run' S207 stack (TKeyword "⟧") = case stack of
    (Smonad (Skeyword (Scocase (Skeyword (Sid stack s x0) _) _ x1) _) _ x2) ->
        gotoMonad s (Smonad stack s (reduction33 x0 x1 x2))
run' S207 stack (TKeyword "|") = case stack of
    (Smonad (Skeyword (Scocase (Skeyword (Sid stack s x0) _) _ x1) _) _ x2) ->
        gotoMonad s (Smonad stack s (reduction33 x0 x1 x2))
run' S207 stack (TKeyword "}") = case stack of
    (Smonad (Skeyword (Scocase (Skeyword (Sid stack s x0) _) _ x1) _) _ x2) ->
        gotoMonad s (Smonad stack s (reduction33 x0 x1 x2))
run' S207 stack (TKeyword "≡") = case stack of
    (Smonad (Skeyword (Scocase (Skeyword (Sid stack s x0) _) _ x1) _) _ x2) ->
        gotoMonad s (Smonad stack s (reduction33 x0 x1 x2))
run' S207 stack (TKeyword ":") = case stack of
    (Smonad (Skeyword (Scocase (Skeyword (Sid stack s x0) _) _ x1) _) _ x2) ->
        gotoMonad s (Smonad stack s (reduction33 x0 x1 x2))
run' S207 stack (TKeyword "⟯") = case stack of
    (Smonad (Skeyword (Scocase (Skeyword (Sid stack s x0) _) _ x1) _) _ x2) ->
        gotoMonad s (Smonad stack s (reduction33 x0 x1 x2))
run' S207 stack (TKeyword "→") = case stack of
    (Smonad (Skeyword (Scocase (Skeyword (Sid stack s x0) _) _ x1) _) _ x2) ->
        gotoMonad s (Smonad stack s (reduction33 x0 x1 x2))
run' S207 stack (TKeyword ",") = case stack of
    (Smonad (Skeyword (Scocase (Skeyword (Sid stack s x0) _) _ x1) _) _ x2) ->
        gotoMonad s (Smonad stack s (reduction33 x0 x1 x2))
run' S208 stack (TKeyword "-") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S208 stack (TKeyword "[") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S208 stack (TKeyword "^") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S208 stack (TKeyword ")") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S208 stack (TKeyword "⟦") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S208 stack (TKeyword "≤") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S208 stack (TKeyword "<") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S208 stack (TKeyword "≠") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S208 stack (TKeyword "{") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S208 stack (TKeyword "⟮") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S208 stack (TReal arg) = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S208 stack (TKeyword "⋆") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S208 stack (TKeyword "*") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S208 stack (TKeyword "def") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S208 stack TEnd = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S208 stack (TInteger arg) = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S208 stack (TKeyword "∘") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S208 stack (TKeyword "⟧") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S208 stack (TKeyword "≥") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S208 stack (TKeyword "|") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S208 stack (TKeyword "=") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S208 stack (TKeyword "≡") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S208 stack (TKeyword "⟯") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S208 stack (TKeyword "→") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S208 stack (TKeyword "+") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S208 stack (TKeyword "⟨") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S208 stack (TKeyword "export") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S208 stack (TId arg) = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S208 stack (TString arg) = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S208 stack (TKeyword "∧") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S208 stack (TKeyword ">") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S208 stack (TKeyword "}") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S208 stack (TKeyword ":") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S208 stack (TKeyword "/") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S208 stack (TKeyword "⊓") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S208 stack (TKeyword "(") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S208 stack (TKeyword "import") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S208 stack (TKeyword "∨") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S208 stack (TKeyword "⟩") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S208 stack (TKeyword "]") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S208 stack (TKeyword "↦") = shift >> run S83 (Skeyword stack S208)
run' S208 stack (TKeyword ";") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S208 stack (TKeyword "⊔") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S208 stack (TKeyword "is") = shift >> run S85 (Skeyword stack S208)
run' S208 stack (TKeyword ",") = case stack of
    (Sid stack s x0) ->
        gotoPrim s (Sprim stack s (reduction72 x0))
run' S209 stack (TKeyword "import") = case stack of
    (Slet (Skeyword (Scocase (Skeyword (Sid stack s x0) _) _ x1) _) _ x2) ->
        gotoLet s (Slet stack s (reduction35 x0 x1 x2))
run' S209 stack (TKeyword "⟧") = case stack of
    (Slet (Skeyword (Scocase (Skeyword (Sid stack s x0) _) _ x1) _) _ x2) ->
        gotoLet s (Slet stack s (reduction35 x0 x1 x2))
run' S209 stack (TKeyword ";") = case stack of
    (Slet (Skeyword (Scocase (Skeyword (Sid stack s x0) _) _ x1) _) _ x2) ->
        gotoLet s (Slet stack s (reduction35 x0 x1 x2))
run' S209 stack (TKeyword "⟩") = case stack of
    (Slet (Skeyword (Scocase (Skeyword (Sid stack s x0) _) _ x1) _) _ x2) ->
        gotoLet s (Slet stack s (reduction35 x0 x1 x2))
run' S209 stack (TKeyword "def") = case stack of
    (Slet (Skeyword (Scocase (Skeyword (Sid stack s x0) _) _ x1) _) _ x2) ->
        gotoLet s (Slet stack s (reduction35 x0 x1 x2))
run' S209 stack (TKeyword "export") = case stack of
    (Slet (Skeyword (Scocase (Skeyword (Sid stack s x0) _) _ x1) _) _ x2) ->
        gotoLet s (Slet stack s (reduction35 x0 x1 x2))
run' S209 stack (TKeyword ")") = case stack of
    (Slet (Skeyword (Scocase (Skeyword (Sid stack s x0) _) _ x1) _) _ x2) ->
        gotoLet s (Slet stack s (reduction35 x0 x1 x2))
run' S209 stack (TKeyword "|") = case stack of
    (Slet (Skeyword (Scocase (Skeyword (Sid stack s x0) _) _ x1) _) _ x2) ->
        gotoLet s (Slet stack s (reduction35 x0 x1 x2))
run' S209 stack (TKeyword "]") = case stack of
    (Slet (Skeyword (Scocase (Skeyword (Sid stack s x0) _) _ x1) _) _ x2) ->
        gotoLet s (Slet stack s (reduction35 x0 x1 x2))
run' S209 stack (TKeyword "}") = case stack of
    (Slet (Skeyword (Scocase (Skeyword (Sid stack s x0) _) _ x1) _) _ x2) ->
        gotoLet s (Slet stack s (reduction35 x0 x1 x2))
run' S209 stack (TKeyword "≡") = case stack of
    (Slet (Skeyword (Scocase (Skeyword (Sid stack s x0) _) _ x1) _) _ x2) ->
        gotoLet s (Slet stack s (reduction35 x0 x1 x2))
run' S209 stack (TKeyword ":") = case stack of
    (Slet (Skeyword (Scocase (Skeyword (Sid stack s x0) _) _ x1) _) _ x2) ->
        gotoLet s (Slet stack s (reduction35 x0 x1 x2))
run' S209 stack TEnd = case stack of
    (Slet (Skeyword (Scocase (Skeyword (Sid stack s x0) _) _ x1) _) _ x2) ->
        gotoLet s (Slet stack s (reduction35 x0 x1 x2))
run' S209 stack (TKeyword "⟯") = case stack of
    (Slet (Skeyword (Scocase (Skeyword (Sid stack s x0) _) _ x1) _) _ x2) ->
        gotoLet s (Slet stack s (reduction35 x0 x1 x2))
run' S209 stack (TKeyword "→") = case stack of
    (Slet (Skeyword (Scocase (Skeyword (Sid stack s x0) _) _ x1) _) _ x2) ->
        gotoLet s (Slet stack s (reduction35 x0 x1 x2))
run' S209 stack (TKeyword ",") = case stack of
    (Slet (Skeyword (Scocase (Skeyword (Sid stack s x0) _) _ x1) _) _ x2) ->
        gotoLet s (Slet stack s (reduction35 x0 x1 x2))
run' S210 stack (TId arg) = shift >> run S101 (Sid stack S210 arg)
run' S211 stack (TKeyword "-") = case stack of
    (Skeyword (Snormal (Skeyword (Slabterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction81 x0 x1))
run' S211 stack (TKeyword "[") = case stack of
    (Skeyword (Snormal (Skeyword (Slabterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction81 x0 x1))
run' S211 stack (TKeyword "^") = case stack of
    (Skeyword (Snormal (Skeyword (Slabterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction81 x0 x1))
run' S211 stack (TKeyword ")") = case stack of
    (Skeyword (Snormal (Skeyword (Slabterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction81 x0 x1))
run' S211 stack (TKeyword "⟦") = case stack of
    (Skeyword (Snormal (Skeyword (Slabterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction81 x0 x1))
run' S211 stack (TKeyword "≤") = case stack of
    (Skeyword (Snormal (Skeyword (Slabterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction81 x0 x1))
run' S211 stack (TKeyword "<") = case stack of
    (Skeyword (Snormal (Skeyword (Slabterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction81 x0 x1))
run' S211 stack (TKeyword "≠") = case stack of
    (Skeyword (Snormal (Skeyword (Slabterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction81 x0 x1))
run' S211 stack (TKeyword "{") = case stack of
    (Skeyword (Snormal (Skeyword (Slabterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction81 x0 x1))
run' S211 stack (TKeyword "⟮") = case stack of
    (Skeyword (Snormal (Skeyword (Slabterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction81 x0 x1))
run' S211 stack (TReal arg) = case stack of
    (Skeyword (Snormal (Skeyword (Slabterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction81 x0 x1))
run' S211 stack (TKeyword "⋆") = case stack of
    (Skeyword (Snormal (Skeyword (Slabterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction81 x0 x1))
run' S211 stack (TKeyword "*") = case stack of
    (Skeyword (Snormal (Skeyword (Slabterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction81 x0 x1))
run' S211 stack (TKeyword "def") = case stack of
    (Skeyword (Snormal (Skeyword (Slabterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction81 x0 x1))
run' S211 stack TEnd = case stack of
    (Skeyword (Snormal (Skeyword (Slabterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction81 x0 x1))
run' S211 stack (TInteger arg) = case stack of
    (Skeyword (Snormal (Skeyword (Slabterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction81 x0 x1))
run' S211 stack (TKeyword "∘") = case stack of
    (Skeyword (Snormal (Skeyword (Slabterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction81 x0 x1))
run' S211 stack (TKeyword "⟧") = case stack of
    (Skeyword (Snormal (Skeyword (Slabterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction81 x0 x1))
run' S211 stack (TKeyword "≥") = case stack of
    (Skeyword (Snormal (Skeyword (Slabterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction81 x0 x1))
run' S211 stack (TKeyword "|") = case stack of
    (Skeyword (Snormal (Skeyword (Slabterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction81 x0 x1))
run' S211 stack (TKeyword "=") = case stack of
    (Skeyword (Snormal (Skeyword (Slabterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction81 x0 x1))
run' S211 stack (TKeyword "≡") = case stack of
    (Skeyword (Snormal (Skeyword (Slabterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction81 x0 x1))
run' S211 stack (TKeyword "⟯") = case stack of
    (Skeyword (Snormal (Skeyword (Slabterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction81 x0 x1))
run' S211 stack (TKeyword "→") = case stack of
    (Skeyword (Snormal (Skeyword (Slabterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction81 x0 x1))
run' S211 stack (TKeyword "+") = case stack of
    (Skeyword (Snormal (Skeyword (Slabterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction81 x0 x1))
run' S211 stack (TKeyword "⟨") = case stack of
    (Skeyword (Snormal (Skeyword (Slabterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction81 x0 x1))
run' S211 stack (TKeyword "export") = case stack of
    (Skeyword (Snormal (Skeyword (Slabterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction81 x0 x1))
run' S211 stack (TId arg) = case stack of
    (Skeyword (Snormal (Skeyword (Slabterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction81 x0 x1))
run' S211 stack (TString arg) = case stack of
    (Skeyword (Snormal (Skeyword (Slabterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction81 x0 x1))
run' S211 stack (TKeyword "∧") = case stack of
    (Skeyword (Snormal (Skeyword (Slabterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction81 x0 x1))
run' S211 stack (TKeyword ">") = case stack of
    (Skeyword (Snormal (Skeyword (Slabterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction81 x0 x1))
run' S211 stack (TKeyword "}") = case stack of
    (Skeyword (Snormal (Skeyword (Slabterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction81 x0 x1))
run' S211 stack (TKeyword ":") = case stack of
    (Skeyword (Snormal (Skeyword (Slabterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction81 x0 x1))
run' S211 stack (TKeyword "/") = case stack of
    (Skeyword (Snormal (Skeyword (Slabterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction81 x0 x1))
run' S211 stack (TKeyword "⊓") = case stack of
    (Skeyword (Snormal (Skeyword (Slabterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction81 x0 x1))
run' S211 stack (TKeyword "(") = case stack of
    (Skeyword (Snormal (Skeyword (Slabterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction81 x0 x1))
run' S211 stack (TKeyword "import") = case stack of
    (Skeyword (Snormal (Skeyword (Slabterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction81 x0 x1))
run' S211 stack (TKeyword "∨") = case stack of
    (Skeyword (Snormal (Skeyword (Slabterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction81 x0 x1))
run' S211 stack (TKeyword "⟩") = case stack of
    (Skeyword (Snormal (Skeyword (Slabterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction81 x0 x1))
run' S211 stack (TKeyword "]") = case stack of
    (Skeyword (Snormal (Skeyword (Slabterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction81 x0 x1))
run' S211 stack (TKeyword ";") = case stack of
    (Skeyword (Snormal (Skeyword (Slabterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction81 x0 x1))
run' S211 stack (TKeyword "⊔") = case stack of
    (Skeyword (Snormal (Skeyword (Slabterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction81 x0 x1))
run' S211 stack (TKeyword ",") = case stack of
    (Skeyword (Snormal (Skeyword (Slabterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction81 x0 x1))
run' S212 stack (TKeyword "⟯") = case stack of
    (Snormal (Skeyword (Sbinders0 (Sid stack s x0) _ x1) _) _ x2) ->
        gotoCase s (Scase stack s (reduction105 x0 x1 x2))
run' S212 stack (TKeyword ",") = case stack of
    (Snormal (Skeyword (Sbinders0 (Sid stack s x0) _ x1) _) _ x2) ->
        gotoCase s (Scase stack s (reduction105 x0 x1 x2))
run' S213 stack (TKeyword "→") = shift >> run S218 (Skeyword stack S213)
run' S214 stack (TKeyword "-") = case stack of
    (Skeyword (Sterm (Skeyword (Sterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction84 x0 x1))
run' S214 stack (TKeyword "[") = case stack of
    (Skeyword (Sterm (Skeyword (Sterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction84 x0 x1))
run' S214 stack (TKeyword "^") = case stack of
    (Skeyword (Sterm (Skeyword (Sterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction84 x0 x1))
run' S214 stack (TKeyword ")") = case stack of
    (Skeyword (Sterm (Skeyword (Sterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction84 x0 x1))
run' S214 stack (TKeyword "⟦") = case stack of
    (Skeyword (Sterm (Skeyword (Sterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction84 x0 x1))
run' S214 stack (TKeyword "≤") = case stack of
    (Skeyword (Sterm (Skeyword (Sterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction84 x0 x1))
run' S214 stack (TKeyword "<") = case stack of
    (Skeyword (Sterm (Skeyword (Sterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction84 x0 x1))
run' S214 stack (TKeyword "≠") = case stack of
    (Skeyword (Sterm (Skeyword (Sterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction84 x0 x1))
run' S214 stack (TKeyword "{") = case stack of
    (Skeyword (Sterm (Skeyword (Sterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction84 x0 x1))
run' S214 stack (TKeyword "⟮") = case stack of
    (Skeyword (Sterm (Skeyword (Sterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction84 x0 x1))
run' S214 stack (TReal arg) = case stack of
    (Skeyword (Sterm (Skeyword (Sterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction84 x0 x1))
run' S214 stack (TKeyword "⋆") = case stack of
    (Skeyword (Sterm (Skeyword (Sterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction84 x0 x1))
run' S214 stack (TKeyword "*") = case stack of
    (Skeyword (Sterm (Skeyword (Sterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction84 x0 x1))
run' S214 stack (TKeyword "def") = case stack of
    (Skeyword (Sterm (Skeyword (Sterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction84 x0 x1))
run' S214 stack TEnd = case stack of
    (Skeyword (Sterm (Skeyword (Sterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction84 x0 x1))
run' S214 stack (TInteger arg) = case stack of
    (Skeyword (Sterm (Skeyword (Sterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction84 x0 x1))
run' S214 stack (TKeyword "∘") = case stack of
    (Skeyword (Sterm (Skeyword (Sterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction84 x0 x1))
run' S214 stack (TKeyword "⟧") = case stack of
    (Skeyword (Sterm (Skeyword (Sterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction84 x0 x1))
run' S214 stack (TKeyword "≥") = case stack of
    (Skeyword (Sterm (Skeyword (Sterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction84 x0 x1))
run' S214 stack (TKeyword "|") = case stack of
    (Skeyword (Sterm (Skeyword (Sterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction84 x0 x1))
run' S214 stack (TKeyword "=") = case stack of
    (Skeyword (Sterm (Skeyword (Sterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction84 x0 x1))
run' S214 stack (TKeyword "≡") = case stack of
    (Skeyword (Sterm (Skeyword (Sterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction84 x0 x1))
run' S214 stack (TKeyword "⟯") = case stack of
    (Skeyword (Sterm (Skeyword (Sterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction84 x0 x1))
run' S214 stack (TKeyword "→") = case stack of
    (Skeyword (Sterm (Skeyword (Sterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction84 x0 x1))
run' S214 stack (TKeyword "+") = case stack of
    (Skeyword (Sterm (Skeyword (Sterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction84 x0 x1))
run' S214 stack (TKeyword "⟨") = case stack of
    (Skeyword (Sterm (Skeyword (Sterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction84 x0 x1))
run' S214 stack (TKeyword "export") = case stack of
    (Skeyword (Sterm (Skeyword (Sterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction84 x0 x1))
run' S214 stack (TId arg) = case stack of
    (Skeyword (Sterm (Skeyword (Sterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction84 x0 x1))
run' S214 stack (TString arg) = case stack of
    (Skeyword (Sterm (Skeyword (Sterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction84 x0 x1))
run' S214 stack (TKeyword "∧") = case stack of
    (Skeyword (Sterm (Skeyword (Sterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction84 x0 x1))
run' S214 stack (TKeyword ">") = case stack of
    (Skeyword (Sterm (Skeyword (Sterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction84 x0 x1))
run' S214 stack (TKeyword "}") = case stack of
    (Skeyword (Sterm (Skeyword (Sterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction84 x0 x1))
run' S214 stack (TKeyword ":") = case stack of
    (Skeyword (Sterm (Skeyword (Sterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction84 x0 x1))
run' S214 stack (TKeyword "/") = case stack of
    (Skeyword (Sterm (Skeyword (Sterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction84 x0 x1))
run' S214 stack (TKeyword "⊓") = case stack of
    (Skeyword (Sterm (Skeyword (Sterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction84 x0 x1))
run' S214 stack (TKeyword "(") = case stack of
    (Skeyword (Sterm (Skeyword (Sterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction84 x0 x1))
run' S214 stack (TKeyword "import") = case stack of
    (Skeyword (Sterm (Skeyword (Sterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction84 x0 x1))
run' S214 stack (TKeyword "∨") = case stack of
    (Skeyword (Sterm (Skeyword (Sterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction84 x0 x1))
run' S214 stack (TKeyword "⟩") = case stack of
    (Skeyword (Sterm (Skeyword (Sterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction84 x0 x1))
run' S214 stack (TKeyword "]") = case stack of
    (Skeyword (Sterm (Skeyword (Sterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction84 x0 x1))
run' S214 stack (TKeyword ";") = case stack of
    (Skeyword (Sterm (Skeyword (Sterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction84 x0 x1))
run' S214 stack (TKeyword "⊔") = case stack of
    (Skeyword (Sterm (Skeyword (Sterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction84 x0 x1))
run' S214 stack (TKeyword ",") = case stack of
    (Skeyword (Sterm (Skeyword (Sterms (Skeyword stack s) _ x0) _) _ x1) _) ->
        gotoPrim s (Sprim stack s (reduction84 x0 x1))
run' S215 stack TEnd = case stack of
    (Sterm (Skeyword (Sterm (Skeyword (Snormal (Skeyword stack s) _ x0) _) _ x1) _) _ x2) ->
        gotoNormal s (Snormal stack s (reduction22 x0 x1 x2))
run' S215 stack (TKeyword ")") = case stack of
    (Sterm (Skeyword (Sterm (Skeyword (Snormal (Skeyword stack s) _ x0) _) _ x1) _) _ x2) ->
        gotoNormal s (Snormal stack s (reduction22 x0 x1 x2))
run' S215 stack (TKeyword "def") = case stack of
    (Sterm (Skeyword (Sterm (Skeyword (Snormal (Skeyword stack s) _ x0) _) _ x1) _) _ x2) ->
        gotoNormal s (Snormal stack s (reduction22 x0 x1 x2))
run' S215 stack (TKeyword "⟩") = case stack of
    (Sterm (Skeyword (Sterm (Skeyword (Snormal (Skeyword stack s) _ x0) _) _ x1) _) _ x2) ->
        gotoNormal s (Snormal stack s (reduction22 x0 x1 x2))
run' S215 stack (TKeyword "export") = case stack of
    (Sterm (Skeyword (Sterm (Skeyword (Snormal (Skeyword stack s) _ x0) _) _ x1) _) _ x2) ->
        gotoNormal s (Snormal stack s (reduction22 x0 x1 x2))
run' S215 stack (TKeyword "import") = case stack of
    (Sterm (Skeyword (Sterm (Skeyword (Snormal (Skeyword stack s) _ x0) _) _ x1) _) _ x2) ->
        gotoNormal s (Snormal stack s (reduction22 x0 x1 x2))
run' S215 stack (TKeyword "⟧") = case stack of
    (Sterm (Skeyword (Sterm (Skeyword (Snormal (Skeyword stack s) _ x0) _) _ x1) _) _ x2) ->
        gotoNormal s (Snormal stack s (reduction22 x0 x1 x2))
run' S215 stack (TKeyword "|") = case stack of
    (Sterm (Skeyword (Sterm (Skeyword (Snormal (Skeyword stack s) _ x0) _) _ x1) _) _ x2) ->
        gotoNormal s (Snormal stack s (reduction22 x0 x1 x2))
run' S215 stack (TKeyword "}") = case stack of
    (Sterm (Skeyword (Sterm (Skeyword (Snormal (Skeyword stack s) _ x0) _) _ x1) _) _ x2) ->
        gotoNormal s (Snormal stack s (reduction22 x0 x1 x2))
run' S215 stack (TKeyword "⟯") = case stack of
    (Sterm (Skeyword (Sterm (Skeyword (Snormal (Skeyword stack s) _ x0) _) _ x1) _) _ x2) ->
        gotoNormal s (Snormal stack s (reduction22 x0 x1 x2))
run' S215 stack (TKeyword ",") = case stack of
    (Sterm (Skeyword (Sterm (Skeyword (Snormal (Skeyword stack s) _ x0) _) _ x1) _) _ x2) ->
        gotoNormal s (Snormal stack s (reduction22 x0 x1 x2))
run' S216 stack (TKeyword "→") = shift >> run S219 (Skeyword stack S216)
run' S217 stack (TKeyword "⟯") = shift >> run S220 (Skeyword stack S217)
run' S218 stack (TKeyword "-") = shift >> run S28 (Skeyword stack S218)
run' S218 stack (TKeyword "⋆") = shift >> run S45 (Skeyword stack S218)
run' S218 stack (TKeyword "⟨") = shift >> run S31 (Skeyword stack S218)
run' S218 stack (TKeyword "λ") = shift >> run S46 (Skeyword stack S218)
run' S218 stack (TKeyword "(") = shift >> run S49 (Skeyword stack S218)
run' S218 stack (TKeyword "∃") = shift >> run S52 (Skeyword stack S218)
run' S218 stack (TKeyword "[") = shift >> run S54 (Skeyword stack S218)
run' S218 stack (TInteger arg) = shift >> run S57 (Sinteger stack S218 arg)
run' S218 stack (TString arg) = shift >> run S36 (Sstring stack S218 arg)
run' S218 stack (TKeyword "⟦") = shift >> run S37 (Skeyword stack S218)
run' S218 stack (TKeyword "~") = shift >> run S59 (Skeyword stack S218)
run' S218 stack (TId arg) = shift >> run S39 (Sid stack S218 arg)
run' S218 stack (TKeyword "{") = shift >> run S42 (Skeyword stack S218)
run' S218 stack (TReal arg) = shift >> run S44 (Sreal stack S218 arg)
run' S218 stack (TKeyword "⟮") = shift >> run S43 (Skeyword stack S218)
run' S218 stack (TKeyword "¬") = shift >> run S63 (Skeyword stack S218)
run' S219 stack (TKeyword "-") = shift >> run S28 (Skeyword stack S219)
run' S219 stack (TKeyword "⋆") = shift >> run S45 (Skeyword stack S219)
run' S219 stack (TKeyword "⟨") = shift >> run S31 (Skeyword stack S219)
run' S219 stack (TKeyword "λ") = shift >> run S46 (Skeyword stack S219)
run' S219 stack (TKeyword "(") = shift >> run S49 (Skeyword stack S219)
run' S219 stack (TKeyword "∃") = shift >> run S52 (Skeyword stack S219)
run' S219 stack (TKeyword "[") = shift >> run S54 (Skeyword stack S219)
run' S219 stack (TInteger arg) = shift >> run S57 (Sinteger stack S219 arg)
run' S219 stack (TString arg) = shift >> run S36 (Sstring stack S219 arg)
run' S219 stack (TKeyword "⟦") = shift >> run S37 (Skeyword stack S219)
run' S219 stack (TKeyword "~") = shift >> run S59 (Skeyword stack S219)
run' S219 stack (TId arg) = shift >> run S39 (Sid stack S219 arg)
run' S219 stack (TKeyword "{") = shift >> run S42 (Skeyword stack S219)
run' S219 stack (TReal arg) = shift >> run S44 (Sreal stack S219 arg)
run' S219 stack (TKeyword "⟮") = shift >> run S43 (Skeyword stack S219)
run' S219 stack (TKeyword "¬") = shift >> run S63 (Skeyword stack S219)
run' S220 stack (TKeyword "-") = case stack of
    (Skeyword (Scases (Skeyword (Skeyword (Snormal (Skeyword (Scall stack s x0) _) _ x1) _) _) _ x2) _) ->
        gotoCall s (Scall stack s (reduction71 x0 x1 x2))
run' S220 stack (TKeyword "[") = case stack of
    (Skeyword (Scases (Skeyword (Skeyword (Snormal (Skeyword (Scall stack s x0) _) _ x1) _) _) _ x2) _) ->
        gotoCall s (Scall stack s (reduction71 x0 x1 x2))
run' S220 stack (TKeyword "^") = case stack of
    (Skeyword (Scases (Skeyword (Skeyword (Snormal (Skeyword (Scall stack s x0) _) _ x1) _) _) _ x2) _) ->
        gotoCall s (Scall stack s (reduction71 x0 x1 x2))
run' S220 stack (TKeyword ")") = case stack of
    (Skeyword (Scases (Skeyword (Skeyword (Snormal (Skeyword (Scall stack s x0) _) _ x1) _) _) _ x2) _) ->
        gotoCall s (Scall stack s (reduction71 x0 x1 x2))
run' S220 stack (TKeyword "⟦") = case stack of
    (Skeyword (Scases (Skeyword (Skeyword (Snormal (Skeyword (Scall stack s x0) _) _ x1) _) _) _ x2) _) ->
        gotoCall s (Scall stack s (reduction71 x0 x1 x2))
run' S220 stack (TKeyword "≤") = case stack of
    (Skeyword (Scases (Skeyword (Skeyword (Snormal (Skeyword (Scall stack s x0) _) _ x1) _) _) _ x2) _) ->
        gotoCall s (Scall stack s (reduction71 x0 x1 x2))
run' S220 stack (TKeyword "<") = case stack of
    (Skeyword (Scases (Skeyword (Skeyword (Snormal (Skeyword (Scall stack s x0) _) _ x1) _) _) _ x2) _) ->
        gotoCall s (Scall stack s (reduction71 x0 x1 x2))
run' S220 stack (TKeyword "≠") = case stack of
    (Skeyword (Scases (Skeyword (Skeyword (Snormal (Skeyword (Scall stack s x0) _) _ x1) _) _) _ x2) _) ->
        gotoCall s (Scall stack s (reduction71 x0 x1 x2))
run' S220 stack (TKeyword "{") = case stack of
    (Skeyword (Scases (Skeyword (Skeyword (Snormal (Skeyword (Scall stack s x0) _) _ x1) _) _) _ x2) _) ->
        gotoCall s (Scall stack s (reduction71 x0 x1 x2))
run' S220 stack (TKeyword "⟮") = case stack of
    (Skeyword (Scases (Skeyword (Skeyword (Snormal (Skeyword (Scall stack s x0) _) _ x1) _) _) _ x2) _) ->
        gotoCall s (Scall stack s (reduction71 x0 x1 x2))
run' S220 stack (TReal arg) = case stack of
    (Skeyword (Scases (Skeyword (Skeyword (Snormal (Skeyword (Scall stack s x0) _) _ x1) _) _) _ x2) _) ->
        gotoCall s (Scall stack s (reduction71 x0 x1 x2))
run' S220 stack (TKeyword "⋆") = case stack of
    (Skeyword (Scases (Skeyword (Skeyword (Snormal (Skeyword (Scall stack s x0) _) _ x1) _) _) _ x2) _) ->
        gotoCall s (Scall stack s (reduction71 x0 x1 x2))
run' S220 stack (TKeyword "*") = case stack of
    (Skeyword (Scases (Skeyword (Skeyword (Snormal (Skeyword (Scall stack s x0) _) _ x1) _) _) _ x2) _) ->
        gotoCall s (Scall stack s (reduction71 x0 x1 x2))
run' S220 stack (TKeyword "def") = case stack of
    (Skeyword (Scases (Skeyword (Skeyword (Snormal (Skeyword (Scall stack s x0) _) _ x1) _) _) _ x2) _) ->
        gotoCall s (Scall stack s (reduction71 x0 x1 x2))
run' S220 stack TEnd = case stack of
    (Skeyword (Scases (Skeyword (Skeyword (Snormal (Skeyword (Scall stack s x0) _) _ x1) _) _) _ x2) _) ->
        gotoCall s (Scall stack s (reduction71 x0 x1 x2))
run' S220 stack (TInteger arg) = case stack of
    (Skeyword (Scases (Skeyword (Skeyword (Snormal (Skeyword (Scall stack s x0) _) _ x1) _) _) _ x2) _) ->
        gotoCall s (Scall stack s (reduction71 x0 x1 x2))
run' S220 stack (TKeyword "∘") = case stack of
    (Skeyword (Scases (Skeyword (Skeyword (Snormal (Skeyword (Scall stack s x0) _) _ x1) _) _) _ x2) _) ->
        gotoCall s (Scall stack s (reduction71 x0 x1 x2))
run' S220 stack (TKeyword "⟧") = case stack of
    (Skeyword (Scases (Skeyword (Skeyword (Snormal (Skeyword (Scall stack s x0) _) _ x1) _) _) _ x2) _) ->
        gotoCall s (Scall stack s (reduction71 x0 x1 x2))
run' S220 stack (TKeyword "≥") = case stack of
    (Skeyword (Scases (Skeyword (Skeyword (Snormal (Skeyword (Scall stack s x0) _) _ x1) _) _) _ x2) _) ->
        gotoCall s (Scall stack s (reduction71 x0 x1 x2))
run' S220 stack (TKeyword "|") = case stack of
    (Skeyword (Scases (Skeyword (Skeyword (Snormal (Skeyword (Scall stack s x0) _) _ x1) _) _) _ x2) _) ->
        gotoCall s (Scall stack s (reduction71 x0 x1 x2))
run' S220 stack (TKeyword "=") = case stack of
    (Skeyword (Scases (Skeyword (Skeyword (Snormal (Skeyword (Scall stack s x0) _) _ x1) _) _) _ x2) _) ->
        gotoCall s (Scall stack s (reduction71 x0 x1 x2))
run' S220 stack (TKeyword "≡") = case stack of
    (Skeyword (Scases (Skeyword (Skeyword (Snormal (Skeyword (Scall stack s x0) _) _ x1) _) _) _ x2) _) ->
        gotoCall s (Scall stack s (reduction71 x0 x1 x2))
run' S220 stack (TKeyword "⟯") = case stack of
    (Skeyword (Scases (Skeyword (Skeyword (Snormal (Skeyword (Scall stack s x0) _) _ x1) _) _) _ x2) _) ->
        gotoCall s (Scall stack s (reduction71 x0 x1 x2))
run' S220 stack (TKeyword "→") = case stack of
    (Skeyword (Scases (Skeyword (Skeyword (Snormal (Skeyword (Scall stack s x0) _) _ x1) _) _) _ x2) _) ->
        gotoCall s (Scall stack s (reduction71 x0 x1 x2))
run' S220 stack (TKeyword "+") = case stack of
    (Skeyword (Scases (Skeyword (Skeyword (Snormal (Skeyword (Scall stack s x0) _) _ x1) _) _) _ x2) _) ->
        gotoCall s (Scall stack s (reduction71 x0 x1 x2))
run' S220 stack (TKeyword "⟨") = case stack of
    (Skeyword (Scases (Skeyword (Skeyword (Snormal (Skeyword (Scall stack s x0) _) _ x1) _) _) _ x2) _) ->
        gotoCall s (Scall stack s (reduction71 x0 x1 x2))
run' S220 stack (TKeyword "export") = case stack of
    (Skeyword (Scases (Skeyword (Skeyword (Snormal (Skeyword (Scall stack s x0) _) _ x1) _) _) _ x2) _) ->
        gotoCall s (Scall stack s (reduction71 x0 x1 x2))
run' S220 stack (TId arg) = case stack of
    (Skeyword (Scases (Skeyword (Skeyword (Snormal (Skeyword (Scall stack s x0) _) _ x1) _) _) _ x2) _) ->
        gotoCall s (Scall stack s (reduction71 x0 x1 x2))
run' S220 stack (TString arg) = case stack of
    (Skeyword (Scases (Skeyword (Skeyword (Snormal (Skeyword (Scall stack s x0) _) _ x1) _) _) _ x2) _) ->
        gotoCall s (Scall stack s (reduction71 x0 x1 x2))
run' S220 stack (TKeyword "∧") = case stack of
    (Skeyword (Scases (Skeyword (Skeyword (Snormal (Skeyword (Scall stack s x0) _) _ x1) _) _) _ x2) _) ->
        gotoCall s (Scall stack s (reduction71 x0 x1 x2))
run' S220 stack (TKeyword ">") = case stack of
    (Skeyword (Scases (Skeyword (Skeyword (Snormal (Skeyword (Scall stack s x0) _) _ x1) _) _) _ x2) _) ->
        gotoCall s (Scall stack s (reduction71 x0 x1 x2))
run' S220 stack (TKeyword "}") = case stack of
    (Skeyword (Scases (Skeyword (Skeyword (Snormal (Skeyword (Scall stack s x0) _) _ x1) _) _) _ x2) _) ->
        gotoCall s (Scall stack s (reduction71 x0 x1 x2))
run' S220 stack (TKeyword ":") = case stack of
    (Skeyword (Scases (Skeyword (Skeyword (Snormal (Skeyword (Scall stack s x0) _) _ x1) _) _) _ x2) _) ->
        gotoCall s (Scall stack s (reduction71 x0 x1 x2))
run' S220 stack (TKeyword "/") = case stack of
    (Skeyword (Scases (Skeyword (Skeyword (Snormal (Skeyword (Scall stack s x0) _) _ x1) _) _) _ x2) _) ->
        gotoCall s (Scall stack s (reduction71 x0 x1 x2))
run' S220 stack (TKeyword "⊓") = case stack of
    (Skeyword (Scases (Skeyword (Skeyword (Snormal (Skeyword (Scall stack s x0) _) _ x1) _) _) _ x2) _) ->
        gotoCall s (Scall stack s (reduction71 x0 x1 x2))
run' S220 stack (TKeyword "(") = case stack of
    (Skeyword (Scases (Skeyword (Skeyword (Snormal (Skeyword (Scall stack s x0) _) _ x1) _) _) _ x2) _) ->
        gotoCall s (Scall stack s (reduction71 x0 x1 x2))
run' S220 stack (TKeyword "import") = case stack of
    (Skeyword (Scases (Skeyword (Skeyword (Snormal (Skeyword (Scall stack s x0) _) _ x1) _) _) _ x2) _) ->
        gotoCall s (Scall stack s (reduction71 x0 x1 x2))
run' S220 stack (TKeyword "∨") = case stack of
    (Skeyword (Scases (Skeyword (Skeyword (Snormal (Skeyword (Scall stack s x0) _) _ x1) _) _) _ x2) _) ->
        gotoCall s (Scall stack s (reduction71 x0 x1 x2))
run' S220 stack (TKeyword "⟩") = case stack of
    (Skeyword (Scases (Skeyword (Skeyword (Snormal (Skeyword (Scall stack s x0) _) _ x1) _) _) _ x2) _) ->
        gotoCall s (Scall stack s (reduction71 x0 x1 x2))
run' S220 stack (TKeyword "]") = case stack of
    (Skeyword (Scases (Skeyword (Skeyword (Snormal (Skeyword (Scall stack s x0) _) _ x1) _) _) _ x2) _) ->
        gotoCall s (Scall stack s (reduction71 x0 x1 x2))
run' S220 stack (TKeyword ";") = case stack of
    (Skeyword (Scases (Skeyword (Skeyword (Snormal (Skeyword (Scall stack s x0) _) _ x1) _) _) _ x2) _) ->
        gotoCall s (Scall stack s (reduction71 x0 x1 x2))
run' S220 stack (TKeyword "⊔") = case stack of
    (Skeyword (Scases (Skeyword (Skeyword (Snormal (Skeyword (Scall stack s x0) _) _ x1) _) _) _ x2) _) ->
        gotoCall s (Scall stack s (reduction71 x0 x1 x2))
run' S220 stack (TKeyword ",") = case stack of
    (Skeyword (Scases (Skeyword (Skeyword (Snormal (Skeyword (Scall stack s x0) _) _ x1) _) _) _ x2) _) ->
        gotoCall s (Scall stack s (reduction71 x0 x1 x2))
run' S221 stack TEnd = case stack of
    (Snormal (Skeyword (Skeyword (Snormal (Skeyword (Sid (Skeyword stack s) _ x0) _) _ x1) _) _) _ x2) ->
        gotoNormal s (Snormal stack s (reduction20 x0 x1 x2))
run' S221 stack (TKeyword ")") = case stack of
    (Snormal (Skeyword (Skeyword (Snormal (Skeyword (Sid (Skeyword stack s) _ x0) _) _ x1) _) _) _ x2) ->
        gotoNormal s (Snormal stack s (reduction20 x0 x1 x2))
run' S221 stack (TKeyword "def") = case stack of
    (Snormal (Skeyword (Skeyword (Snormal (Skeyword (Sid (Skeyword stack s) _ x0) _) _ x1) _) _) _ x2) ->
        gotoNormal s (Snormal stack s (reduction20 x0 x1 x2))
run' S221 stack (TKeyword "⟩") = case stack of
    (Snormal (Skeyword (Skeyword (Snormal (Skeyword (Sid (Skeyword stack s) _ x0) _) _ x1) _) _) _ x2) ->
        gotoNormal s (Snormal stack s (reduction20 x0 x1 x2))
run' S221 stack (TKeyword "export") = case stack of
    (Snormal (Skeyword (Skeyword (Snormal (Skeyword (Sid (Skeyword stack s) _ x0) _) _ x1) _) _) _ x2) ->
        gotoNormal s (Snormal stack s (reduction20 x0 x1 x2))
run' S221 stack (TKeyword "import") = case stack of
    (Snormal (Skeyword (Skeyword (Snormal (Skeyword (Sid (Skeyword stack s) _ x0) _) _ x1) _) _) _ x2) ->
        gotoNormal s (Snormal stack s (reduction20 x0 x1 x2))
run' S221 stack (TKeyword "⟧") = case stack of
    (Snormal (Skeyword (Skeyword (Snormal (Skeyword (Sid (Skeyword stack s) _ x0) _) _ x1) _) _) _ x2) ->
        gotoNormal s (Snormal stack s (reduction20 x0 x1 x2))
run' S221 stack (TKeyword "|") = case stack of
    (Snormal (Skeyword (Skeyword (Snormal (Skeyword (Sid (Skeyword stack s) _ x0) _) _ x1) _) _) _ x2) ->
        gotoNormal s (Snormal stack s (reduction20 x0 x1 x2))
run' S221 stack (TKeyword "}") = case stack of
    (Snormal (Skeyword (Skeyword (Snormal (Skeyword (Sid (Skeyword stack s) _ x0) _) _ x1) _) _) _ x2) ->
        gotoNormal s (Snormal stack s (reduction20 x0 x1 x2))
run' S221 stack (TKeyword "⟯") = case stack of
    (Snormal (Skeyword (Skeyword (Snormal (Skeyword (Sid (Skeyword stack s) _ x0) _) _ x1) _) _) _ x2) ->
        gotoNormal s (Snormal stack s (reduction20 x0 x1 x2))
run' S221 stack (TKeyword ",") = case stack of
    (Snormal (Skeyword (Skeyword (Snormal (Skeyword (Sid (Skeyword stack s) _ x0) _) _ x1) _) _) _ x2) ->
        gotoNormal s (Snormal stack s (reduction20 x0 x1 x2))
run' S222 stack TEnd = case stack of
    (Snormal (Skeyword (Skeyword (Snormal (Skeyword (Sid (Skeyword (Sstructdef stack s x0) _) _ x1) _) _ x2) _) _) _ x3) ->
        gotoNormal s (Snormal stack s (reduction19 x0 x1 x2 x3))
run' S222 stack (TKeyword ")") = case stack of
    (Snormal (Skeyword (Skeyword (Snormal (Skeyword (Sid (Skeyword (Sstructdef stack s x0) _) _ x1) _) _ x2) _) _) _ x3) ->
        gotoNormal s (Snormal stack s (reduction19 x0 x1 x2 x3))
run' S222 stack (TKeyword "def") = case stack of
    (Snormal (Skeyword (Skeyword (Snormal (Skeyword (Sid (Skeyword (Sstructdef stack s x0) _) _ x1) _) _ x2) _) _) _ x3) ->
        gotoNormal s (Snormal stack s (reduction19 x0 x1 x2 x3))
run' S222 stack (TKeyword "⟩") = case stack of
    (Snormal (Skeyword (Skeyword (Snormal (Skeyword (Sid (Skeyword (Sstructdef stack s x0) _) _ x1) _) _ x2) _) _) _ x3) ->
        gotoNormal s (Snormal stack s (reduction19 x0 x1 x2 x3))
run' S222 stack (TKeyword "export") = case stack of
    (Snormal (Skeyword (Skeyword (Snormal (Skeyword (Sid (Skeyword (Sstructdef stack s x0) _) _ x1) _) _ x2) _) _) _ x3) ->
        gotoNormal s (Snormal stack s (reduction19 x0 x1 x2 x3))
run' S222 stack (TKeyword "import") = case stack of
    (Snormal (Skeyword (Skeyword (Snormal (Skeyword (Sid (Skeyword (Sstructdef stack s x0) _) _ x1) _) _ x2) _) _) _ x3) ->
        gotoNormal s (Snormal stack s (reduction19 x0 x1 x2 x3))
run' S222 stack (TKeyword "⟧") = case stack of
    (Snormal (Skeyword (Skeyword (Snormal (Skeyword (Sid (Skeyword (Sstructdef stack s x0) _) _ x1) _) _ x2) _) _) _ x3) ->
        gotoNormal s (Snormal stack s (reduction19 x0 x1 x2 x3))
run' S222 stack (TKeyword "|") = case stack of
    (Snormal (Skeyword (Skeyword (Snormal (Skeyword (Sid (Skeyword (Sstructdef stack s x0) _) _ x1) _) _ x2) _) _) _ x3) ->
        gotoNormal s (Snormal stack s (reduction19 x0 x1 x2 x3))
run' S222 stack (TKeyword "}") = case stack of
    (Snormal (Skeyword (Skeyword (Snormal (Skeyword (Sid (Skeyword (Sstructdef stack s x0) _) _ x1) _) _ x2) _) _) _ x3) ->
        gotoNormal s (Snormal stack s (reduction19 x0 x1 x2 x3))
run' S222 stack (TKeyword "⟯") = case stack of
    (Snormal (Skeyword (Skeyword (Snormal (Skeyword (Sid (Skeyword (Sstructdef stack s x0) _) _ x1) _) _ x2) _) _) _ x3) ->
        gotoNormal s (Snormal stack s (reduction19 x0 x1 x2 x3))
run' S222 stack (TKeyword ",") = case stack of
    (Snormal (Skeyword (Skeyword (Snormal (Skeyword (Sid (Skeyword (Sstructdef stack s x0) _) _ x1) _) _ x2) _) _) _ x3) ->
        gotoNormal s (Snormal stack s (reduction19 x0 x1 x2 x3))

gotoPrim :: Status a -> (Cprim a) -> Parser (Maybe [Statement])
gotoPrim S16 = run S29
gotoPrim S17 = run S29
gotoPrim S27 = run S29
gotoPrim S28 = run S29
gotoPrim S37 = run S29
gotoPrim S38 = run S29
gotoPrim S42 = run S29
gotoPrim S49 = run S29
gotoPrim S54 = run S29
gotoPrim S59 = run S29
gotoPrim S63 = run S29
gotoPrim S76 = run S29
gotoPrim S77 = run S29
gotoPrim S78 = run S29
gotoPrim S79 = run S29
gotoPrim S81 = run S29
gotoPrim S83 = run S29
gotoPrim S84 = run S29
gotoPrim S85 = run S29
gotoPrim S86 = run S29
gotoPrim S87 = run S29
gotoPrim S88 = run S144
gotoPrim S89 = run S29
gotoPrim S90 = run S29
gotoPrim S96 = run S29
gotoPrim S103 = run S29
gotoPrim S104 = run S29
gotoPrim S105 = run S29
gotoPrim S106 = run S29
gotoPrim S107 = run S29
gotoPrim S108 = run S29
gotoPrim S114 = run S29
gotoPrim S117 = run S29
gotoPrim S118 = run S29
gotoPrim S119 = run S29
gotoPrim S120 = run S29
gotoPrim S121 = run S29
gotoPrim S122 = run S29
gotoPrim S125 = run S29
gotoPrim S130 = run S29
gotoPrim S135 = run S29
gotoPrim S137 = run S29
gotoPrim S151 = run S29
gotoPrim S152 = run S29
gotoPrim S155 = run S29
gotoPrim S160 = run S29
gotoPrim S167 = run S29
gotoPrim S169 = run S29
gotoPrim S174 = run S29
gotoPrim S185 = run S29
gotoPrim S187 = run S29
gotoPrim S188 = run S29
gotoPrim S199 = run S29
gotoPrim S205 = run S29
gotoPrim S218 = run S29
gotoPrim S219 = run S29
gotoPrim _ = \_ -> pure Nothing

gotoMonad :: Status a -> (Cmonad a) -> Parser (Maybe [Statement])
gotoMonad S16 = run S30
gotoMonad S17 = run S30
gotoMonad S27 = run S30
gotoMonad S28 = run S30
gotoMonad S37 = run S30
gotoMonad S38 = run S30
gotoMonad S42 = run S30
gotoMonad S49 = run S30
gotoMonad S54 = run S30
gotoMonad S81 = run S30
gotoMonad S87 = run S30
gotoMonad S90 = run S30
gotoMonad S96 = run S30
gotoMonad S114 = run S172
gotoMonad S120 = run S30
gotoMonad S121 = run S30
gotoMonad S122 = run S30
gotoMonad S130 = run S30
gotoMonad S135 = run S30
gotoMonad S137 = run S30
gotoMonad S151 = run S30
gotoMonad S152 = run S30
gotoMonad S155 = run S30
gotoMonad S160 = run S30
gotoMonad S167 = run S30
gotoMonad S169 = run S30
gotoMonad S174 = run S30
gotoMonad S185 = run S30
gotoMonad S187 = run S207
gotoMonad S199 = run S30
gotoMonad S205 = run S30
gotoMonad S218 = run S30
gotoMonad S219 = run S30
gotoMonad _ = \_ -> pure Nothing

gotoConj2 :: Status a -> (Cconj2 a) -> Parser (Maybe [Statement])
gotoConj2 S16 = run S60
gotoConj2 S17 = run S60
gotoConj2 S27 = run S60
gotoConj2 S28 = run S60
gotoConj2 S37 = run S60
gotoConj2 S38 = run S60
gotoConj2 S42 = run S60
gotoConj2 S49 = run S60
gotoConj2 S54 = run S60
gotoConj2 S76 = run S131
gotoConj2 S81 = run S60
gotoConj2 S83 = run S60
gotoConj2 S84 = run S60
gotoConj2 S85 = run S60
gotoConj2 S87 = run S60
gotoConj2 S90 = run S60
gotoConj2 S96 = run S60
gotoConj2 S114 = run S60
gotoConj2 S120 = run S60
gotoConj2 S121 = run S60
gotoConj2 S122 = run S60
gotoConj2 S125 = run S60
gotoConj2 S130 = run S60
gotoConj2 S135 = run S60
gotoConj2 S137 = run S60
gotoConj2 S151 = run S60
gotoConj2 S152 = run S60
gotoConj2 S155 = run S60
gotoConj2 S160 = run S60
gotoConj2 S167 = run S60
gotoConj2 S169 = run S60
gotoConj2 S174 = run S60
gotoConj2 S185 = run S60
gotoConj2 S187 = run S60
gotoConj2 S188 = run S60
gotoConj2 S199 = run S60
gotoConj2 S205 = run S60
gotoConj2 S218 = run S60
gotoConj2 S219 = run S60
gotoConj2 _ = \_ -> pure Nothing

gotoLabterm :: Status a -> (Clabterm a) -> Parser (Maybe [Statement])
gotoLabterm S42 = run S97
gotoLabterm S90 = run S97
gotoLabterm S154 = run S97
gotoLabterm _ = \_ -> pure Nothing

gotoPrefix :: Status a -> (Cprefix a) -> Parser (Maybe [Statement])
gotoPrefix S16 = run S33
gotoPrefix S17 = run S33
gotoPrefix S27 = run S33
gotoPrefix S28 = run S33
gotoPrefix S37 = run S33
gotoPrefix S38 = run S33
gotoPrefix S42 = run S33
gotoPrefix S49 = run S33
gotoPrefix S54 = run S33
gotoPrefix S59 = run S123
gotoPrefix S63 = run S33
gotoPrefix S76 = run S33
gotoPrefix S77 = run S33
gotoPrefix S78 = run S33
gotoPrefix S79 = run S33
gotoPrefix S81 = run S33
gotoPrefix S83 = run S33
gotoPrefix S84 = run S33
gotoPrefix S85 = run S33
gotoPrefix S86 = run S33
gotoPrefix S87 = run S33
gotoPrefix S90 = run S33
gotoPrefix S96 = run S33
gotoPrefix S103 = run S33
gotoPrefix S104 = run S33
gotoPrefix S105 = run S33
gotoPrefix S106 = run S33
gotoPrefix S107 = run S33
gotoPrefix S108 = run S33
gotoPrefix S114 = run S33
gotoPrefix S117 = run S33
gotoPrefix S118 = run S33
gotoPrefix S119 = run S33
gotoPrefix S120 = run S33
gotoPrefix S121 = run S33
gotoPrefix S122 = run S33
gotoPrefix S125 = run S33
gotoPrefix S130 = run S33
gotoPrefix S135 = run S33
gotoPrefix S137 = run S33
gotoPrefix S151 = run S33
gotoPrefix S152 = run S33
gotoPrefix S155 = run S33
gotoPrefix S160 = run S33
gotoPrefix S167 = run S33
gotoPrefix S169 = run S33
gotoPrefix S174 = run S33
gotoPrefix S185 = run S33
gotoPrefix S187 = run S33
gotoPrefix S188 = run S33
gotoPrefix S199 = run S33
gotoPrefix S205 = run S33
gotoPrefix S218 = run S33
gotoPrefix S219 = run S33
gotoPrefix _ = \_ -> pure Nothing

gotoNames :: Status a -> (Cnames a) -> Parser (Maybe [Statement])
gotoNames S4 = run S8
gotoNames S12 = run S21
gotoNames S19 = run S66
gotoNames _ = \_ -> pure Nothing

gotoFile :: Status a -> (Cfile a) -> Parser (Maybe [Statement])
gotoFile S0 = run S5
gotoFile _ = \_ -> pure Nothing

gotoConj :: Status a -> (Cconj a) -> Parser (Maybe [Statement])
gotoConj S16 = run S34
gotoConj S17 = run S34
gotoConj S27 = run S34
gotoConj S28 = run S34
gotoConj S37 = run S34
gotoConj S38 = run S34
gotoConj S42 = run S34
gotoConj S49 = run S34
gotoConj S54 = run S34
gotoConj S76 = run S34
gotoConj S79 = run S34
gotoConj S81 = run S34
gotoConj S83 = run S34
gotoConj S84 = run S34
gotoConj S85 = run S34
gotoConj S87 = run S34
gotoConj S90 = run S34
gotoConj S96 = run S34
gotoConj S114 = run S34
gotoConj S119 = run S177
gotoConj S120 = run S34
gotoConj S121 = run S34
gotoConj S122 = run S34
gotoConj S125 = run S34
gotoConj S130 = run S34
gotoConj S135 = run S34
gotoConj S137 = run S34
gotoConj S151 = run S34
gotoConj S152 = run S34
gotoConj S155 = run S34
gotoConj S160 = run S34
gotoConj S167 = run S34
gotoConj S169 = run S34
gotoConj S174 = run S34
gotoConj S185 = run S34
gotoConj S187 = run S34
gotoConj S188 = run S34
gotoConj S199 = run S34
gotoConj S205 = run S34
gotoConj S218 = run S34
gotoConj S219 = run S34
gotoConj _ = \_ -> pure Nothing

gotoDisj :: Status a -> (Cdisj a) -> Parser (Maybe [Statement])
gotoDisj S16 = run S35
gotoDisj S17 = run S35
gotoDisj S27 = run S35
gotoDisj S28 = run S35
gotoDisj S37 = run S35
gotoDisj S38 = run S35
gotoDisj S42 = run S35
gotoDisj S49 = run S35
gotoDisj S54 = run S35
gotoDisj S81 = run S35
gotoDisj S83 = run S35
gotoDisj S84 = run S35
gotoDisj S85 = run S35
gotoDisj S87 = run S35
gotoDisj S90 = run S35
gotoDisj S96 = run S35
gotoDisj S114 = run S35
gotoDisj S120 = run S35
gotoDisj S121 = run S35
gotoDisj S122 = run S35
gotoDisj S125 = run S181
gotoDisj S130 = run S35
gotoDisj S135 = run S35
gotoDisj S137 = run S35
gotoDisj S151 = run S35
gotoDisj S152 = run S35
gotoDisj S155 = run S35
gotoDisj S160 = run S35
gotoDisj S167 = run S35
gotoDisj S169 = run S35
gotoDisj S174 = run S35
gotoDisj S185 = run S35
gotoDisj S187 = run S35
gotoDisj S188 = run S35
gotoDisj S199 = run S35
gotoDisj S205 = run S35
gotoDisj S218 = run S35
gotoDisj S219 = run S35
gotoDisj _ = \_ -> pure Nothing

gotoStruct :: Status a -> (Cstruct a) -> Parser (Maybe [Statement])
gotoStruct S41 = run S88
gotoStruct _ = \_ -> pure Nothing

gotoLabterms :: Status a -> (Clabterms a) -> Parser (Maybe [Statement])
gotoLabterms S42 = run S92
gotoLabterms S90 = run S146
gotoLabterms S154 = run S196
gotoLabterms _ = \_ -> pure Nothing

gotoStructdef :: Status a -> (Cstructdef a) -> Parser (Maybe [Statement])
gotoStructdef S16 = run S38
gotoStructdef S17 = run S38
gotoStructdef S27 = run S38
gotoStructdef S28 = run S38
gotoStructdef S37 = run S38
gotoStructdef S49 = run S38
gotoStructdef S81 = run S38
gotoStructdef S87 = run S38
gotoStructdef S96 = run S38
gotoStructdef S121 = run S38
gotoStructdef S122 = run S38
gotoStructdef S130 = run S38
gotoStructdef S137 = run S38
gotoStructdef S151 = run S38
gotoStructdef S152 = run S38
gotoStructdef S167 = run S38
gotoStructdef S185 = run S38
gotoStructdef S199 = run S38
gotoStructdef S218 = run S38
gotoStructdef S219 = run S38
gotoStructdef _ = \_ -> pure Nothing

gotoLabbinds :: Status a -> (Clabbinds a) -> Parser (Maybe [Statement])
gotoLabbinds S13 = run S22
gotoLabbinds S69 = run S127
gotoLabbinds _ = \_ -> pure Nothing

gotoLambdabinders :: Status a -> (Clambdabinders a) -> Parser (Maybe [Statement])
gotoLambdabinders S7 = run S15
gotoLambdabinders S18 = run S65
gotoLambdabinders S46 = run S102
gotoLambdabinders _ = \_ -> pure Nothing

gotoBinders :: Status a -> (Cbinders a) -> Parser (Maybe [Statement])
gotoBinders S52 = run S111
gotoBinders S101 = run S158
gotoBinders S113 = run S171
gotoBinders S170 = run S203
gotoBinders _ = \_ -> pure Nothing

gotoAdd :: Status a -> (Cadd a) -> Parser (Maybe [Statement])
gotoAdd S16 = run S40
gotoAdd S17 = run S40
gotoAdd S27 = run S40
gotoAdd S28 = run S40
gotoAdd S37 = run S40
gotoAdd S38 = run S40
gotoAdd S42 = run S40
gotoAdd S49 = run S40
gotoAdd S54 = run S40
gotoAdd S63 = run S40
gotoAdd S76 = run S40
gotoAdd S79 = run S40
gotoAdd S81 = run S40
gotoAdd S83 = run S40
gotoAdd S84 = run S40
gotoAdd S85 = run S40
gotoAdd S86 = run S40
gotoAdd S87 = run S40
gotoAdd S90 = run S40
gotoAdd S96 = run S40
gotoAdd S103 = run S40
gotoAdd S104 = run S40
gotoAdd S105 = run S40
gotoAdd S106 = run S40
gotoAdd S107 = run S40
gotoAdd S108 = run S40
gotoAdd S114 = run S40
gotoAdd S117 = run S175
gotoAdd S118 = run S176
gotoAdd S119 = run S40
gotoAdd S120 = run S40
gotoAdd S121 = run S40
gotoAdd S122 = run S40
gotoAdd S125 = run S40
gotoAdd S130 = run S40
gotoAdd S135 = run S40
gotoAdd S137 = run S40
gotoAdd S151 = run S40
gotoAdd S152 = run S40
gotoAdd S155 = run S40
gotoAdd S160 = run S40
gotoAdd S167 = run S40
gotoAdd S169 = run S40
gotoAdd S174 = run S40
gotoAdd S185 = run S40
gotoAdd S187 = run S40
gotoAdd S188 = run S40
gotoAdd S199 = run S40
gotoAdd S205 = run S40
gotoAdd S218 = run S40
gotoAdd S219 = run S40
gotoAdd _ = \_ -> pure Nothing

gotoCall :: Status a -> (Ccall a) -> Parser (Maybe [Statement])
gotoCall S16 = run S41
gotoCall S17 = run S41
gotoCall S27 = run S41
gotoCall S28 = run S41
gotoCall S37 = run S41
gotoCall S38 = run S41
gotoCall S42 = run S41
gotoCall S49 = run S41
gotoCall S54 = run S41
gotoCall S59 = run S41
gotoCall S63 = run S41
gotoCall S76 = run S41
gotoCall S77 = run S41
gotoCall S78 = run S41
gotoCall S79 = run S41
gotoCall S81 = run S41
gotoCall S83 = run S41
gotoCall S84 = run S41
gotoCall S85 = run S41
gotoCall S86 = run S41
gotoCall S87 = run S41
gotoCall S89 = run S41
gotoCall S90 = run S41
gotoCall S96 = run S41
gotoCall S103 = run S41
gotoCall S104 = run S41
gotoCall S105 = run S41
gotoCall S106 = run S41
gotoCall S107 = run S41
gotoCall S108 = run S41
gotoCall S114 = run S41
gotoCall S117 = run S41
gotoCall S118 = run S41
gotoCall S119 = run S41
gotoCall S120 = run S41
gotoCall S121 = run S41
gotoCall S122 = run S41
gotoCall S125 = run S41
gotoCall S130 = run S41
gotoCall S135 = run S41
gotoCall S137 = run S41
gotoCall S151 = run S41
gotoCall S152 = run S41
gotoCall S155 = run S41
gotoCall S160 = run S41
gotoCall S167 = run S41
gotoCall S169 = run S41
gotoCall S174 = run S41
gotoCall S185 = run S41
gotoCall S187 = run S41
gotoCall S188 = run S41
gotoCall S199 = run S41
gotoCall S205 = run S41
gotoCall S218 = run S41
gotoCall S219 = run S41
gotoCall _ = \_ -> pure Nothing

gotoStatement :: Status a -> (Cstatement a) -> Parser (Maybe [Statement])
gotoStatement S0 = run S3
gotoStatement S5 = run S11
gotoStatement _ = \_ -> pure Nothing

gotoMul :: Status a -> (Cmul a) -> Parser (Maybe [Statement])
gotoMul S16 = run S55
gotoMul S17 = run S55
gotoMul S27 = run S55
gotoMul S28 = run S55
gotoMul S37 = run S55
gotoMul S38 = run S55
gotoMul S42 = run S55
gotoMul S49 = run S55
gotoMul S54 = run S55
gotoMul S63 = run S55
gotoMul S76 = run S55
gotoMul S77 = run S132
gotoMul S78 = run S133
gotoMul S79 = run S55
gotoMul S81 = run S55
gotoMul S83 = run S55
gotoMul S84 = run S55
gotoMul S85 = run S55
gotoMul S86 = run S55
gotoMul S87 = run S55
gotoMul S90 = run S55
gotoMul S96 = run S55
gotoMul S103 = run S55
gotoMul S104 = run S55
gotoMul S105 = run S55
gotoMul S106 = run S55
gotoMul S107 = run S55
gotoMul S108 = run S55
gotoMul S114 = run S55
gotoMul S117 = run S55
gotoMul S118 = run S55
gotoMul S119 = run S55
gotoMul S120 = run S55
gotoMul S121 = run S55
gotoMul S122 = run S55
gotoMul S125 = run S55
gotoMul S130 = run S55
gotoMul S135 = run S55
gotoMul S137 = run S55
gotoMul S151 = run S55
gotoMul S152 = run S55
gotoMul S155 = run S55
gotoMul S160 = run S55
gotoMul S167 = run S55
gotoMul S169 = run S55
gotoMul S174 = run S55
gotoMul S185 = run S55
gotoMul S187 = run S55
gotoMul S188 = run S55
gotoMul S199 = run S55
gotoMul S205 = run S55
gotoMul S218 = run S55
gotoMul S219 = run S55
gotoMul _ = \_ -> pure Nothing

gotoLambdabinder :: Status a -> (Clambdabinder a) -> Parser (Maybe [Statement])
gotoLambdabinder S7 = run S18
gotoLambdabinder S18 = run S18
gotoLambdabinder S46 = run S18
gotoLambdabinder _ = \_ -> pure Nothing

gotoTerms :: Status a -> (Cterms a) -> Parser (Maybe [Statement])
gotoTerms S42 = run S95
gotoTerms S54 = run S115
gotoTerms S90 = run S148
gotoTerms S155 = run S197
gotoTerms _ = \_ -> pure Nothing

gotoNormal :: Status a -> (Cnormal a) -> Parser (Maybe [Statement])
gotoNormal S16 = run S47
gotoNormal S17 = run S64
gotoNormal S27 = run S70
gotoNormal S28 = run S71
gotoNormal S37 = run S80
gotoNormal S49 = run S110
gotoNormal S81 = run S110
gotoNormal S87 = run S143
gotoNormal S96 = run S110
gotoNormal S121 = run S179
gotoNormal S122 = run S180
gotoNormal S130 = run S183
gotoNormal S137 = run S186
gotoNormal S151 = run S193
gotoNormal S152 = run S194
gotoNormal S167 = run S201
gotoNormal S185 = run S206
gotoNormal S199 = run S212
gotoNormal S218 = run S221
gotoNormal S219 = run S222
gotoNormal _ = \_ -> pure Nothing

gotoComp :: Status a -> (Ccomp a) -> Parser (Maybe [Statement])
gotoComp S16 = run S48
gotoComp S17 = run S48
gotoComp S27 = run S48
gotoComp S28 = run S48
gotoComp S37 = run S48
gotoComp S38 = run S48
gotoComp S42 = run S48
gotoComp S49 = run S48
gotoComp S54 = run S48
gotoComp S63 = run S48
gotoComp S76 = run S48
gotoComp S79 = run S48
gotoComp S81 = run S48
gotoComp S83 = run S48
gotoComp S84 = run S48
gotoComp S85 = run S48
gotoComp S86 = run S142
gotoComp S87 = run S48
gotoComp S90 = run S48
gotoComp S96 = run S48
gotoComp S103 = run S161
gotoComp S104 = run S162
gotoComp S105 = run S163
gotoComp S106 = run S164
gotoComp S107 = run S165
gotoComp S108 = run S166
gotoComp S114 = run S48
gotoComp S119 = run S48
gotoComp S120 = run S48
gotoComp S121 = run S48
gotoComp S122 = run S48
gotoComp S125 = run S48
gotoComp S130 = run S48
gotoComp S135 = run S48
gotoComp S137 = run S48
gotoComp S151 = run S48
gotoComp S152 = run S48
gotoComp S155 = run S48
gotoComp S160 = run S48
gotoComp S167 = run S48
gotoComp S169 = run S48
gotoComp S174 = run S48
gotoComp S185 = run S48
gotoComp S187 = run S48
gotoComp S188 = run S48
gotoComp S199 = run S48
gotoComp S205 = run S48
gotoComp S218 = run S48
gotoComp S219 = run S48
gotoComp _ = \_ -> pure Nothing

gotoStructbind :: Status a -> (Cstructbind a) -> Parser (Maybe [Statement])
gotoStructbind S7 = run S14
gotoStructbind S18 = run S14
gotoStructbind S46 = run S14
gotoStructbind _ = \_ -> pure Nothing

gotoEquals :: Status a -> (Cequals a) -> Parser (Maybe [Statement])
gotoEquals S16 = run S51
gotoEquals S17 = run S51
gotoEquals S27 = run S51
gotoEquals S28 = run S51
gotoEquals S37 = run S51
gotoEquals S38 = run S51
gotoEquals S42 = run S51
gotoEquals S49 = run S51
gotoEquals S54 = run S51
gotoEquals S63 = run S51
gotoEquals S76 = run S51
gotoEquals S79 = run S51
gotoEquals S81 = run S51
gotoEquals S83 = run S51
gotoEquals S84 = run S51
gotoEquals S85 = run S51
gotoEquals S87 = run S51
gotoEquals S90 = run S51
gotoEquals S96 = run S51
gotoEquals S114 = run S51
gotoEquals S119 = run S51
gotoEquals S120 = run S51
gotoEquals S121 = run S51
gotoEquals S122 = run S51
gotoEquals S125 = run S51
gotoEquals S130 = run S51
gotoEquals S135 = run S51
gotoEquals S137 = run S51
gotoEquals S151 = run S51
gotoEquals S152 = run S51
gotoEquals S155 = run S51
gotoEquals S160 = run S51
gotoEquals S167 = run S51
gotoEquals S169 = run S51
gotoEquals S174 = run S51
gotoEquals S185 = run S51
gotoEquals S187 = run S51
gotoEquals S188 = run S51
gotoEquals S199 = run S51
gotoEquals S205 = run S51
gotoEquals S218 = run S51
gotoEquals S219 = run S51
gotoEquals _ = \_ -> pure Nothing

gotoLet :: Status a -> (Clet a) -> Parser (Maybe [Statement])
gotoLet S16 = run S53
gotoLet S17 = run S53
gotoLet S27 = run S53
gotoLet S28 = run S53
gotoLet S37 = run S53
gotoLet S38 = run S53
gotoLet S42 = run S53
gotoLet S49 = run S53
gotoLet S54 = run S53
gotoLet S81 = run S53
gotoLet S87 = run S53
gotoLet S90 = run S53
gotoLet S96 = run S53
gotoLet S114 = run S53
gotoLet S120 = run S53
gotoLet S121 = run S53
gotoLet S122 = run S53
gotoLet S130 = run S53
gotoLet S135 = run S53
gotoLet S137 = run S53
gotoLet S151 = run S53
gotoLet S152 = run S53
gotoLet S155 = run S53
gotoLet S160 = run S53
gotoLet S167 = run S53
gotoLet S169 = run S53
gotoLet S174 = run S53
gotoLet S185 = run S53
gotoLet S187 = run S53
gotoLet S188 = run S209
gotoLet S199 = run S53
gotoLet S205 = run S53
gotoLet S218 = run S53
gotoLet S219 = run S53
gotoLet _ = \_ -> pure Nothing

gotoBinders0 :: Status a -> (Cbinders0 a) -> Parser (Maybe [Statement])
gotoBinders0 S101 = run S159
gotoBinders0 _ = \_ -> pure Nothing

gotoNot :: Status a -> (Cnot a) -> Parser (Maybe [Statement])
gotoNot S16 = run S56
gotoNot S17 = run S56
gotoNot S27 = run S56
gotoNot S28 = run S56
gotoNot S37 = run S56
gotoNot S38 = run S56
gotoNot S42 = run S56
gotoNot S49 = run S56
gotoNot S54 = run S56
gotoNot S63 = run S126
gotoNot S76 = run S56
gotoNot S79 = run S56
gotoNot S81 = run S56
gotoNot S83 = run S56
gotoNot S84 = run S56
gotoNot S85 = run S56
gotoNot S87 = run S56
gotoNot S90 = run S56
gotoNot S96 = run S56
gotoNot S114 = run S56
gotoNot S119 = run S56
gotoNot S120 = run S56
gotoNot S121 = run S56
gotoNot S122 = run S56
gotoNot S125 = run S56
gotoNot S130 = run S56
gotoNot S135 = run S56
gotoNot S137 = run S56
gotoNot S151 = run S56
gotoNot S152 = run S56
gotoNot S155 = run S56
gotoNot S160 = run S56
gotoNot S167 = run S56
gotoNot S169 = run S56
gotoNot S174 = run S56
gotoNot S185 = run S56
gotoNot S187 = run S56
gotoNot S188 = run S56
gotoNot S199 = run S56
gotoNot S205 = run S56
gotoNot S218 = run S56
gotoNot S219 = run S56
gotoNot _ = \_ -> pure Nothing

gotoLabtypes :: Status a -> (Clabtypes a) -> Parser (Maybe [Statement])
gotoLabtypes S31 = run S72
gotoLabtypes S129 = run S182
gotoLabtypes _ = \_ -> pure Nothing

gotoCases :: Status a -> (Ccases a) -> Parser (Maybe [Statement])
gotoCases S43 = run S100
gotoCases S91 = run S149
gotoCases S156 = run S198
gotoCases S210 = run S217
gotoCases _ = \_ -> pure Nothing

gotoCase :: Status a -> (Ccase a) -> Parser (Maybe [Statement])
gotoCase S43 = run S99
gotoCase S91 = run S99
gotoCase S156 = run S99
gotoCase S210 = run S99
gotoCase _ = \_ -> pure Nothing

gotoTerm :: Status a -> (Cterm a) -> Parser (Maybe [Statement])
gotoTerm S16 = run S58
gotoTerm S17 = run S58
gotoTerm S27 = run S58
gotoTerm S28 = run S58
gotoTerm S37 = run S58
gotoTerm S38 = run S82
gotoTerm S42 = run S98
gotoTerm S49 = run S58
gotoTerm S54 = run S98
gotoTerm S81 = run S58
gotoTerm S87 = run S58
gotoTerm S90 = run S98
gotoTerm S96 = run S58
gotoTerm S120 = run S178
gotoTerm S121 = run S58
gotoTerm S122 = run S58
gotoTerm S130 = run S58
gotoTerm S135 = run S184
gotoTerm S137 = run S58
gotoTerm S151 = run S58
gotoTerm S152 = run S58
gotoTerm S155 = run S98
gotoTerm S167 = run S58
gotoTerm S174 = run S204
gotoTerm S185 = run S58
gotoTerm S199 = run S58
gotoTerm S205 = run S215
gotoTerm S218 = run S58
gotoTerm S219 = run S58
gotoTerm _ = \_ -> pure Nothing

gotoCocase :: Status a -> (Ccocase a) -> Parser (Maybe [Statement])
gotoCocase S16 = run S50
gotoCocase S17 = run S50
gotoCocase S27 = run S50
gotoCocase S28 = run S50
gotoCocase S37 = run S50
gotoCocase S38 = run S50
gotoCocase S42 = run S50
gotoCocase S49 = run S50
gotoCocase S54 = run S50
gotoCocase S81 = run S50
gotoCocase S83 = run S139
gotoCocase S84 = run S140
gotoCocase S85 = run S141
gotoCocase S87 = run S50
gotoCocase S90 = run S50
gotoCocase S96 = run S50
gotoCocase S114 = run S50
gotoCocase S120 = run S50
gotoCocase S121 = run S50
gotoCocase S122 = run S50
gotoCocase S130 = run S50
gotoCocase S135 = run S50
gotoCocase S137 = run S50
gotoCocase S151 = run S50
gotoCocase S152 = run S50
gotoCocase S155 = run S50
gotoCocase S160 = run S50
gotoCocase S167 = run S50
gotoCocase S169 = run S50
gotoCocase S174 = run S50
gotoCocase S185 = run S50
gotoCocase S187 = run S50
gotoCocase S188 = run S50
gotoCocase S199 = run S50
gotoCocase S205 = run S50
gotoCocase S218 = run S50
gotoCocase S219 = run S50
gotoCocase _ = \_ -> pure Nothing

gotoName :: Status a -> (Cname a) -> Parser (Maybe [Statement])
gotoName S4 = run S9
gotoName S12 = run S9
gotoName S19 = run S9
gotoName _ = \_ -> pure Nothing

gotoDisj2 :: Status a -> (Cdisj2 a) -> Parser (Maybe [Statement])
gotoDisj2 S16 = run S32
gotoDisj2 S17 = run S32
gotoDisj2 S27 = run S32
gotoDisj2 S28 = run S32
gotoDisj2 S37 = run S32
gotoDisj2 S38 = run S32
gotoDisj2 S42 = run S32
gotoDisj2 S49 = run S32
gotoDisj2 S54 = run S32
gotoDisj2 S76 = run S32
gotoDisj2 S79 = run S134
gotoDisj2 S81 = run S32
gotoDisj2 S83 = run S32
gotoDisj2 S84 = run S32
gotoDisj2 S85 = run S32
gotoDisj2 S87 = run S32
gotoDisj2 S90 = run S32
gotoDisj2 S96 = run S32
gotoDisj2 S114 = run S32
gotoDisj2 S120 = run S32
gotoDisj2 S121 = run S32
gotoDisj2 S122 = run S32
gotoDisj2 S125 = run S32
gotoDisj2 S130 = run S32
gotoDisj2 S135 = run S32
gotoDisj2 S137 = run S32
gotoDisj2 S151 = run S32
gotoDisj2 S152 = run S32
gotoDisj2 S155 = run S32
gotoDisj2 S160 = run S32
gotoDisj2 S167 = run S32
gotoDisj2 S169 = run S32
gotoDisj2 S174 = run S32
gotoDisj2 S185 = run S32
gotoDisj2 S187 = run S32
gotoDisj2 S188 = run S32
gotoDisj2 S199 = run S32
gotoDisj2 S205 = run S32
gotoDisj2 S218 = run S32
gotoDisj2 S219 = run S32
gotoDisj2 _ = \_ -> pure Nothing

gotoLabtype :: Status a -> (Clabtype a) -> Parser (Maybe [Statement])
gotoLabtype S31 = run S73
gotoLabtype S129 = run S73
gotoLabtype _ = \_ -> pure Nothing

gotoExp :: Status a -> (Cexp a) -> Parser (Maybe [Statement])
gotoExp S16 = run S61
gotoExp S17 = run S61
gotoExp S27 = run S61
gotoExp S28 = run S61
gotoExp S37 = run S61
gotoExp S38 = run S61
gotoExp S42 = run S61
gotoExp S49 = run S61
gotoExp S54 = run S61
gotoExp S59 = run S61
gotoExp S63 = run S61
gotoExp S76 = run S61
gotoExp S77 = run S61
gotoExp S78 = run S61
gotoExp S79 = run S61
gotoExp S81 = run S61
gotoExp S83 = run S61
gotoExp S84 = run S61
gotoExp S85 = run S61
gotoExp S86 = run S61
gotoExp S87 = run S61
gotoExp S89 = run S145
gotoExp S90 = run S61
gotoExp S96 = run S61
gotoExp S103 = run S61
gotoExp S104 = run S61
gotoExp S105 = run S61
gotoExp S106 = run S61
gotoExp S107 = run S61
gotoExp S108 = run S61
gotoExp S114 = run S61
gotoExp S117 = run S61
gotoExp S118 = run S61
gotoExp S119 = run S61
gotoExp S120 = run S61
gotoExp S121 = run S61
gotoExp S122 = run S61
gotoExp S125 = run S61
gotoExp S130 = run S61
gotoExp S135 = run S61
gotoExp S137 = run S61
gotoExp S151 = run S61
gotoExp S152 = run S61
gotoExp S155 = run S61
gotoExp S160 = run S61
gotoExp S167 = run S61
gotoExp S169 = run S61
gotoExp S174 = run S61
gotoExp S185 = run S61
gotoExp S187 = run S61
gotoExp S188 = run S61
gotoExp S199 = run S61
gotoExp S205 = run S61
gotoExp S218 = run S61
gotoExp S219 = run S61
gotoExp _ = \_ -> pure Nothing

gotoLabbind :: Status a -> (Clabbind a) -> Parser (Maybe [Statement])
gotoLabbind S13 = run S25
gotoLabbind S69 = run S25
gotoLabbind _ = \_ -> pure Nothing

gotoLambda :: Status a -> (Clambda a) -> Parser (Maybe [Statement])
gotoLambda S16 = run S62
gotoLambda S17 = run S62
gotoLambda S27 = run S62
gotoLambda S28 = run S62
gotoLambda S37 = run S62
gotoLambda S38 = run S62
gotoLambda S42 = run S62
gotoLambda S49 = run S62
gotoLambda S54 = run S62
gotoLambda S81 = run S62
gotoLambda S87 = run S62
gotoLambda S90 = run S62
gotoLambda S96 = run S62
gotoLambda S120 = run S62
gotoLambda S121 = run S62
gotoLambda S122 = run S62
gotoLambda S130 = run S62
gotoLambda S135 = run S62
gotoLambda S137 = run S62
gotoLambda S151 = run S62
gotoLambda S152 = run S62
gotoLambda S155 = run S62
gotoLambda S160 = run S200
gotoLambda S167 = run S62
gotoLambda S169 = run S202
gotoLambda S174 = run S62
gotoLambda S185 = run S62
gotoLambda S199 = run S62
gotoLambda S205 = run S62
gotoLambda S218 = run S62
gotoLambda S219 = run S62
gotoLambda _ = \_ -> pure Nothing

data Empty = SEmpty
data Ckeyword a = Skeyword a (Status a)
data Cinteger a = Sinteger a (Status a) Integer
data Cstring a = Sstring a (Status a) String
data Cid a = Sid a (Status a) String
data Creal a = Sreal a (Status a) String

data Cprim a          = Sprim a (Status a) Tprim
data Cmonad a         = Smonad a (Status a) Tmonad
data Cconj2 a         = Sconj2 a (Status a) Tconj2
data Clabterm a       = Slabterm a (Status a) Tlabterm
data Cprefix a        = Sprefix a (Status a) Tprefix
data Cnames a         = Snames a (Status a) Tnames
data Cfile a          = Sfile a (Status a) Tfile
data Cconj a          = Sconj a (Status a) Tconj
data Cdisj a          = Sdisj a (Status a) Tdisj
data Cstruct a        = Sstruct a (Status a) Tstruct
data Clabterms a      = Slabterms a (Status a) Tlabterms
data Cstructdef a     = Sstructdef a (Status a) Tstructdef
data Clabbinds a      = Slabbinds a (Status a) Tlabbinds
data Clambdabinders a = Slambdabinders a (Status a) Tlambdabinders
data Cbinders a       = Sbinders a (Status a) Tbinders
data Cadd a           = Sadd a (Status a) Tadd
data Ccall a          = Scall a (Status a) Tcall
data Cstatement a     = Sstatement a (Status a) Tstatement
data Cmul a           = Smul a (Status a) Tmul
data Clambdabinder a  = Slambdabinder a (Status a) Tlambdabinder
data Cterms a         = Sterms a (Status a) Tterms
data Cnormal a        = Snormal a (Status a) Tnormal
data Ccomp a          = Scomp a (Status a) Tcomp
data Cstructbind a    = Sstructbind a (Status a) Tstructbind
data Cequals a        = Sequals a (Status a) Tequals
data Clet a           = Slet a (Status a) Tlet
data Cbinders0 a      = Sbinders0 a (Status a) Tbinders0
data Cnot a           = Snot a (Status a) Tnot
data Clabtypes a      = Slabtypes a (Status a) Tlabtypes
data Ccases a         = Scases a (Status a) Tcases
data Ccase a          = Scase a (Status a) Tcase
data Cterm a          = Sterm a (Status a) Tterm
data Ccocase a        = Scocase a (Status a) Tcocase
data Cname a          = Sname a (Status a) Tname
data Cdisj2 a         = Sdisj2 a (Status a) Tdisj2
data Clabtype a       = Slabtype a (Status a) Tlabtype
data Cexp a           = Sexp a (Status a) Texp
data Clabbind a       = Slabbind a (Status a) Tlabbind
data Clambda a        = Slambda a (Status a) Tlambda

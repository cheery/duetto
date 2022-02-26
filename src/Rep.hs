module Rep where

data Statement
    = Import String [(String, String)]
    | Export [(String,String)]
    | TypeDecl String STerm
    | Decl String STerm
    deriving (Show,Eq)

data STerm = SName String
           | SCall STerm [(String, STerm)] STerm
           | SAbs [String] String STerm
           | SPi [(String, STerm)] String STerm STerm
           | SAnn STerm STerm
           | SHole
           | SStar
           | SEq STerm STerm STerm
           | SIntegerConst Integer
           | SRealConst String
           | SStringConst String
           | SBind String STerm STerm
           | SField STerm String
           | SCase STerm STerm [(String, STerm)]
           | SCocase [(String, STerm)]
           | SORecord [STerm]
           | SLRecord [(String, STerm)] (Maybe STerm)
    deriving (Show,Eq)

-- #lexemes id, integer, real, string
-- # data Statements
-- DataDecl = lambda name, parms, strucs: obj.Data(4, [name, parms, strucs])
-- CodataDecl = lambda name, parms, strucs: obj.Data(5, [name, parms, strucs])
-- RecordDecl = lambda head, parms, strucs: obj.Data(6, [head, parms, strucs])
-- ClassDecl = lambda head, parms, strucs: obj.Data(7, [head, parms, strucs])
-- 
-- # data Term
-- Rigid = lambda rigid,trail: obj.Data(0, [rigid, trail])
-- Flex = lambda meta,trail: obj.Data(1, [meta, trail])
-- Abs = lambda body: obj.Data(2, [body])
-- Pi = lambda a, b, hidden: obj.Data(3, [a, b, hidden])
-- Star = obj.Data(4, [])
-- Cocase = lambda binds: obj.Data(5, [binds])
-- Record = lambda fields: obj.Data(6, [fields])
-- Eq = lambda a,x,y: obj.Data(7, [a,x,y])
-- Let = lambda arg,body: obj.Data(8, [arg,body])
-- IntegerConst = lambda num: obj.Data(9, [num])
-- RealConst = lambda num: obj.Data(10, [num])
-- StringConst = lambda string: obj.Data(11, [string])

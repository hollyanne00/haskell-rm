module Util (
	Instruct(Rplus,Rminus,Halt),
	fatbr,
	thinbr,
	codelist,
	codeinst,
	codeilist,
	execute,
	run,
	compute
) where

import Debug.Trace

-- Define instructions as Halt, Ri+=>Lj or Ri-=>Lj,Lk 
data Instruct = Rplus Int Int | Rminus Int Int Int | Halt
-- Define program as a list of instructions

-- Compute fat brackets
fatbr (x,y) = 2^x * (2*y + 1)

-- Compute thin brackets
thinbr (x,y) = 2^x * (2*y + 1) - 1

-- Compute code of a list
codelist [] = 0
codelist (x:xs) = fatbr (x, codelist xs) 

-- Define code of instructions
codeinst Halt = 0
codeinst (Rplus i j) = fatbr(2*i,j)
codeinst (Rminus i j k) = fatbr(2*i + 1, thinbr (j, k)) 

-- Define code of a list of instructions
codeilist [] = 0
codeilist (x:xs) = fatbr (codeinst x, codeilist xs)

-- Execute and instruction, returning updated program counter and registers
execute (Halt) rs  pc = (rs, -1)
execute (Rplus i j) rs pc = let r = (head (take 1 (drop i rs))) in ((take i rs) ++ (r+1) : (drop (i+1) rs), j)
execute (Rminus i j k) rs pc = let r = (head (take 1 (drop i rs)))-1 in ((take i rs) ++ (if r<0 then 0 else r) : (drop (i+1) rs), (if r>=0 then j else k))

-- Recursively execute instructions according to program counter showing the register stages
run prog (rs,pc) = trace ("rs: " ++ show rs ++ " pc: " ++ show pc) $ let (nrs,npc) = (execute (prog !! pc) rs pc) in if npc>(-1) then run prog (nrs,npc) else rs

-- Run a program
compute prog rs = run prog (rs,0)
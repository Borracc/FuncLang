--readParseFile.hs
import System.IO
import ParseProgramSecondaParte

readF:: IO String
readF = do {inh <- openFile "input4.txt" ReadMode;
    prog <- readloop inh;
    hClose inh;
    return prog}

--main--
main::IO(Program Name)
main=do {inp <- readF;
		putStr inp;  -- mostra programma originale
		putChar '\n';
		putChar '\n';
		return (comp(parse parseProg inp))}  -- chiamata funzione parseProg

comp::[(Program Name, Name)] -> Program Name
comp []       = error "no parse"
comp [(e,[])] = e
comp [(_,a)]  = error ("doesn't use all input"++ a)

readloop inh = do {ineof <- hIsEOF inh;
			   if ineof
					then return []
					else do{
						  x<-hGetLine inh;
						  xs<-readloop inh;
						  return (x++xs)}}
--firstPart.hs--

parseProg::Parser(Program Name)
parseProg = do p <- parseScDef
			  do character ';'
				ps <- parseProg
				return(p:ps)
			  <|>return [p]
			  
parseScDef::Parser(ScDef Name)
parseScDef = do v <- parseVar
			    pf <- many parseVar
				character '='
				body <- parseExpr --call to parseExpr
				return (v,pf,body)
				
parseExpr::Parser(Expr Name)

parseAExpr::Parser(Expr Name)

parseDef::Parser(Def Name)

parseAlt::Parser(Alter Name)


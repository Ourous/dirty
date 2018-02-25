implementation module parser

import types, utilities, converter, StdEnv, StdLib, Text

parseUTF8 :: !String -> Program
parseUTF8 string 
	= parseNative {#i \\ c <- utf8ToUnicode string, i <- ['\0'..] & u <- unicodeCharset | c == u}
		
parseNative :: !String -> Program
parseNative string = let
	tokens = map fromString (split "\n" string)
	commands = linkLoops (map (map toCommand) tokens)
	in {
		dimension = {x=last(sort(map length tokens)), y=length tokens},
		source = {{#el \\ el <- line} \\ line <- tokens},
		commands = {{el \\ el <- line} \\ line <- commands},
		wrapping = 0 < sum[1 \\ (Control Terminate) <- flatten commands]
		}
		
linkLoops :: ![[Command]] -> [[Command]]
linkLoops commands = map (map fst) (linkLoops` annotated)
where

	annotated = [[(command, {x=x, y=y}) \\ x <- [0..] & command <- line] \\ y <- [0..] & line <- commands]
	
	linkLoops` = linkLoop Left o linkLoop Right o linkGoto
	
linkLoop _ val = val // TODO

linkGoto commands
	= (map (linkEast o linkWest) o transpose o map (linkNorth o linkSouth) o transpose) commands
where

	linkEast list
		= [(matchWest item (rotateList i list), pos) \\ (item, pos) <- list & i <- [0..]]
	where
	
		matchWest (Control (Goto East Nothing)) list
			= let (Just (_, loc)) = find (\(e, _) -> case e of (Control (Goto West _)) = True; _ = False) (reverse list)
			in (Control (Goto East (Just loc)))

		matchWest val _ = val
		
	linkWest list
		# list = reverse list
		= reverse [(matchEast item (rotateList i list), pos) \\ (item, pos) <- list & i <- [0..]]
	where
	
		matchEast (Control (Goto West Nothing)) list
			= let (Just (_, loc)) = find (\(e, _) -> case e of (Control (Goto East _)) = True; _ = False) (reverse list)
			in (Control (Goto West (Just loc)))
			
		matchEast val _ = val
		
	linkNorth list
		= [(matchSouth item (rotateList i list), pos) \\ (item, pos) <- list & i <- [0..]]
	where
	
		matchSouth (Control (Goto North Nothing)) list
			= let (Just (_, loc)) = find (\(e, _) -> case e of (Control (Goto South _)) = True; _ = False) (reverse list)
			in (Control (Goto North (Just loc)))

		matchSouth val _ = val
		
	linkSouth list
		# list = reverse list
		= reverse [(matchNorth item (rotateList i list), pos) \\ (item, pos) <- list & i <- [0..]]
	where
	
		matchNorth (Control (Goto South Nothing)) list
			= let (Just (_, loc)) = find (\(e, _) -> case e of (Control (Goto North _)) = True; _ = False) (reverse list)
			in (Control (Goto South (Just loc)))

		matchNorth val _ = val
		
		
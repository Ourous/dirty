implementation module environment

import types, utilities, arithmetic, unicode, stacks
import StdEnv, StdLib, System.Environment, System.Time

getEnvVariable :: *(!Memory, *World) -> *(Memory, *World)
getEnvVariable (memory, world)
	# (main, above) = decons memory.above
	# (Just mid, main) = decons main
	# envName = unicodeToUTF8 (map toInt (toList mid))
	# (envVal, world) = getEnvironmentVariable envName world
	| isNothing envVal
		= ({memory&above=recons(fallback main, above)}, world)
	| otherwise
		# envStr = [!fromInt el \\ el <- utf8ToUnicode (fromJust envVal)]
		= ({memory&above=recons (recons (Just (fromStrictList envStr True), main), above)}, world)

CLOCK_FACTOR =: CLK_PER_SEC / 100
		
sleepFor :: *(!Memory, *World) -> *(Memory, *World)
sleepFor  arg = arg/*
sleepFor mw=:({main={stack=[!El{stack=[!]}:_]}},_) = mw
sleepFor (memory, world)
	# (El mid, other) = decons memory.main
	# (top, mid) = decons mid
	# duration = toInt (abs top)
	# (Clock current, world) = clock world
	# world = sleepUntil (current + duration * CLOCK_FACTOR) world
	= ({memory&main=recons (El mid, other)}, world)
where
	sleepUntil target world
		# (Clock current, world) = clock world
		| current >= target
			= world
		| otherwise
			= sleepUntil target world*/
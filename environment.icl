implementation module environment

import types, utilities, arithmetic, unicode
import StdEnv, StdLib, System.Environment, System.Time

getEnvVariable :: *(!Memory, *World) -> *(Memory, *World)
getEnvVariable (memory=:{main=[El mid:other]}, world)
	# envName = unicodeToUTF8 (map toInt mid)
	# (envVal, world) = getEnvironmentVariable envName world
	| isNothing envVal
		= ({memory&main=[El []:other]}, world)
	| otherwise
		# envStr = map fromInt (utf8ToUnicode (fromJust envVal))
		= ({memory&main=[El envStr:other]}, world)
		
CLOCK_FACTOR =: CLK_PER_SEC / 100
		
sleepFor :: *(!Memory, *World) -> *(Memory, *World)
sleepFor mw=:({main=[El[]:_]},_) = mw
sleepFor (memory=:{main=[El [top:mid]:other]}, world)
	# duration = toInt (abs top)
	# (Clock current, world) = clock world
	# world = sleepUntil (current + duration * CLOCK_FACTOR) world
	= ({memory&main=[El mid:other]}, world)
where
	sleepUntil target world
		# (Clock current, world) = clock world
		| current >= target
			= world
		| otherwise
			= sleepUntil target world
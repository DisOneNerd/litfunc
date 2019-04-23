{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

import Data.String.Interpolate
import Prelude hiding (lookup)
import qualified Data.Map as Map

data Valence =
    MNO
    | PRL
    | CRO
    | RCP
    | CPL
    | NNR
    | DUP
    | DEM
    | RES
    | IMT
    | CNG
    | PTI
    | IDC
    | MUT
    deriving (Show,Eq,Ord)

data Pattern =
    Pat1
    | Pat2
    | Pat3
    deriving (Show, Eq, Ord)

data SDT =
    -- Misc/Uncertain
    GrammPsn
    | Abst
    | Symbols
    -- Bodies
    | Disease
    -- Geometric
    | Dim2
    | Dim3
    | Component
    | GeomAspect
    -- Materials
    | Gas
    | Liquid
    | Solid
    | MatterProperty
    -- Spatial-Temporal
    | DimSpaceTime
    | Orientation
    | Path
    | Season
    | Region
    -- Consumable Items
    | Clothing
    | Food
    | Cooking
    | Tool
    -- Sensory-Cog
    | Emotions
    | Sensory
    | Color
    | Audial
    | Texture
    -- Social Manifest
    | Relations
    | Person
    -- Generic Intellectual
    | Number
    | Knowledge
    | Philosophy
    -- Animals
    | Terra
    | Marine
    | Avian
    | Reptile
    | Insect
    -- Non-Animal
    | Branch
    | NoBranch
    | Fungus
    | Lesser
    deriving(Show,Eq,Ord)

tableSDT_to_String :: [(SDT,String)]
tableSDT_to_String =
 [  (GrammPsn, "nu"),
        (Abst,"spu"),
        (Symbols,"li"),

        (Disease,"vai"),

        (Dim2,"ple"),
        (Dim3,"ra"),
        (Component,"vga"),
        (GeomAspect,"kru"),

        (Gas,"plu"),
        (Liquid,"gi"),
        (Solid,"nsi"),
        (MatterProperty,"mba"),

        (DimSpaceTime,"tra"),
        (Orientation,"vu"), 
        (Path,"vaz"),
        (Season,"si"),
        (Region,"mai"),

        (Clothing,"lau"),
        (Food,"fu"),
        (Cooking,"gra"),
        (Tool,"twi"),

        (Emotions,"rai"),
        (Sensory,"lu"),
        (Color,"ku"),
        (Audial,"nda"),
        (Texture,"fa"),

        (Relations,"ka"),
        (Person,"gai"),

        (Number,"na"),
        (Knowledge,"su"),
        (Philosophy,"ta"),

        (Terra,"ma"),
        (Marine,"sna"),
        (Avian,"vni"),
        (Reptile,"fao"),
        (Insect,"bi"),

        (Branch,"pa"),
        (NoBranch,"kai"),
        (Fungus,"du"),
        (Lesser,"vi")
        ]

-- Confiugrational Affixes are next
-- I usually think of tese in triples of CA Categories

-- PSE
type CaPSE = (Perspective, Structure, Extension)

data Perspective =
  Monadic
  | Unbounded
  | Nomic
  | Abstract
  deriving(Show,Eq,Ord)

data Structure = 
  Uniplex
-- Duplex -- What about k-plex?
  | Discrete
  | Sequential
  | Segmentive
  deriving(Show,Eq,Ord)

data Extension =
  Delimitive
  | Proximal
  | Inceptive
  | Terminative
  | Depletive
  | Graduative
  deriving(Show,Eq,Ord)

----
grabPSE :: Formative -> CaPSE
grabPSE Formative{_structure=strct, _extension = ext, _perspective=psp} =
 ( psp, strct, ext)

tablePerspective :: [(Perspective,String)]
tablePerspective = [ (Monadic,"s"),(Unbounded,"l"), (Nomic,"n"), (Abstract,"r")]
( mapPSP_Str, mapStr_PSP ) = doubleAssociate tablePerspective

tableStructure :: [(Structure,String)]
tableStructure = [ (Uniplex,"e"), (Discrete,"a"),(Sequential,"i"),(Segmentive,"u")]
( mapSTRCT_Str, mapStr_STRCT ) = doubleAssociate tableStructure

tableExtension :: [(Extension,String)]
tableExtension = [(Delimitive,""), (Proximal,"ps"),(Inceptive,"ft"), (Terminative,"tx"), (Depletive,"sk"), (Graduative,"kf")]
( mapEXT_Str, mapStr_EXT ) = doubleAssociate tableExtension

-----
convertPSP_To_String :: Perspective -> String
convertPSP_To_String psp = convertMaybeToString $ convertKeyToVal psp mapPSP_Str

convertSTRCT_To_String :: Structure -> String
convertSTRCT_To_String strct = convertMaybeToString $ convertKeyToVal strct mapSTRCT_Str

convertEXT_To_String :: Extension -> String
convertEXT_To_String ext = convertMaybeToString $ convertKeyToVal ext mapEXT_Str

convertPSE_To_String :: CaPSE -> String
convertPSE_To_String (p,s,e) = [i|#{convertPSP_To_String p}#{convertSTRCT_To_String s}#{convertEXT_To_String e}|]
----



-- VFA
type CaVFA = (Variance,Fuzziness,Affiliation)
data Variance =
  Homogenous
  | Mediary
  | Heterogenous
  deriving (Show, Ord, Eq)

data Fuzziness =
  Distinct
  | Blurry
  | Incoherent
  deriving (Show, Ord, Eq)

data Affiliation = 
  NoSignificance
  | Harmonious
  | Discordial
  deriving (Show, Ord, Eq)
-----
grabVFA :: Formative -> CaVFA
grabVFA Formative{_variance=var, _fuzziness = fuzz, _affiliation = affl} =
 ( var, fuzz, affl)

tableVariance :: [(Variance,String)]
tableVariance = [ (Homogenous,"e"),(Mediary,"au"), (Heterogenous,"ui")]
( mapVAR_Str, mapStr_VAR ) = doubleAssociate tableVariance

tableFuzziness :: [(Fuzziness,String)]
tableFuzziness = [(Distinct, ""), (Blurry,"n"), (Incoherent,"r")]
( mapFUZ_Str, mapStr_FUZ) = doubleAssociate tableFuzziness

tableAffiliation :: [(Affiliation, String)]
tableAffiliation = [(NoSignificance, ""),(Harmonious, "s"), (Discordial,"t")]
(mapAFL_Str, mapStr_AFL) = doubleAssociate tableAffiliation
----
convertVAR_To_String :: Variance -> String
convertVAR_To_String var = convertMaybeToString $ convertKeyToVal var mapVAR_Str

convertFUZ_To_String :: Fuzziness -> String
convertFUZ_To_String fuz = convertMaybeToString $ convertKeyToVal fuz mapFUZ_Str

convertAFL_To_String :: Affiliation -> String
convertAFL_To_String afl = convertMaybeToString $ convertKeyToVal afl mapAFL_Str

convertVFA_To_String :: CaVFA -> String
convertVFA_To_String (v,f,a) = [i|#{convertVAR_To_String v}#{convertFUZ_To_String f}#{convertAFL_To_String a}|]
----


type Cr = String
type Vowel = String

data Formative = Formative
    { _valence :: Valence
    ,   _pattern :: Pattern
    , _sdt :: SDT
    , _vowel1 :: Vowel
    , _vowel2 :: Vowel
    , _root :: Cr
    
    , _variance :: Variance
    , _fuzziness:: Fuzziness
    , _affiliation :: Affiliation

    , _structure :: Structure
    , _extension :: Extension
    , _perspective :: Perspective
    }
    deriving (Show, Ord, Eq)

data SimpleFormative = SimpleFormative
    { 
    _pattern' :: Pattern
    , _sdt' :: SDT
    , _vowel1' :: Vowel
    , _vowel2' :: Vowel
    , _root' :: Cr
}
    deriving ( Show, Eq)

( mapSDT_to_String, mapString_to_SDT ) = doubleAssociate tableSDT_to_String

-- Utilities
convertMaybeToString :: Maybe String -> String
convertMaybeToString (Just x) = x
convertMaybeToString Nothing = []

paramToString :: (Ord p) => p -> Map.Map p String -> String
paramToString p m = convertMaybeToString $ convertKeyToVal p m

convertSDT_To_String :: SDT -> String
convertSDT_To_String sdt = convertMaybeToString $ convertKeyToVal sdt mapSDT_to_String

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

transposeMap :: (Ord k, Ord a) => Map.Map k a -> Map.Map a k
transposeMap = Map.fromList . map swap . Map.toList

doubleAssociate :: (Ord a, Ord b) => [ ( a, b) ] -> (Map.Map a b ,Map.Map b a )
doubleAssociate arr = ( Map.fromList arr , transposeMap $ Map.fromList arr )

convertKeyToVal :: (Ord k) => k -> Map.Map k v ->  Maybe v
convertKeyToVal x m = Map.lookup x m

-- For testing purposes
sample :: Formative
sample = Formative
    { _valence = MNO,
        _pattern = Pat1,
        _sdt = Disease,
        _vowel1 = "ú",
        _vowel2 = "u",
        _root = "nn",

        _structure = Sequential,
        _extension = Delimitive,
        _perspective = Monadic,

        _variance = Homogenous,
        _fuzziness = Distinct,
        _affiliation = NoSignificance
}

sample2 :: Formative
sample2 = Formative {
 _valence = MNO,
_pattern = Pat1,
_sdt = Sensory,
_vowel1 = "á",
_vowel2 = "a",
_root = "zvz",

_perspective = Nomic,
_structure = Uniplex,
_extension = Delimitive,

_variance = Homogenous,
_fuzziness = Distinct,
_affiliation = NoSignificance
}

hello :: Formative
hello = Formative {
    _valence = MNO,
    _pattern = Pat2,
    _sdt = Relations,
    _vowel1="á",
    _vowel2="i",
    _root ="lt",
    
    _perspective = Nomic,
    _structure= Uniplex,
    _extension = Inceptive,

    _variance = Homogenous,
    _fuzziness=Distinct,
    _affiliation=NoSignificance
}


bye :: Formative
bye = Formative {
    _valence = MNO,
    _pattern = Pat2,
    _sdt = Relations,
    _vowel1="á",
    _vowel2="i",
    _root ="lt",
    
    _perspective = Nomic,
    _structure= Uniplex,
    _extension = Terminative,

    _variance = Homogenous,
    _fuzziness=Distinct,
    _affiliation=NoSignificance
}

-- test :: Formative -> String
-- test Formative{_root=_root} = "The root is" ++ show _root
 
grabCore :: Formative -> SimpleFormative
grabCore Formative{_pattern=pattern,
                                     _sdt=sdt,
                                     _vowel1=vowel1,
                                     _vowel2 = vowel2,
                                     _root=root}

 = SimpleFormative{_pattern' = pattern,
                                    _sdt' = sdt,
                                    _vowel1'=vowel1,
                                    _vowel2' = vowel2,
                                    _root' = root}

easy = grabCore sample


consonantChunks :: SimpleFormative -> (String , String)
-- This will take in a simple formative and "chunk" it's root into 2 components
-- This is useful for `convertSimpleFormativeToString` and other functions that arrange root according to pattern
consonantChunks SimpleFormative{_root' = rt}
    | length rt == 2 = (init rt, tail rt)
    | length rt == 3 = (init $ init rt, tail rt)
    | otherwise = (init $ init rt, tail $ tail rt)
    -- I wrote it this way so that they yield string data type and just in case something WEIRD happens
    -- "WEIRD" as in, something bizarre with the indecies occurs.

arrangeStringPattern1 :: SimpleFormative -> String
arrangeStringPattern1 (SimpleFormative {..} )
    = let (c1,c2) = consonantChunks SimpleFormative{_pattern',_sdt',_vowel1',_vowel2',_root'} 
    -- in let strSDT = convertSDT_To_String SimpleFormative{_sdt'} 
            in [i|#{convertSDT_To_String _sdt'}#{c1}#{_vowel1'}#{c2}#{_vowel2'}|]

arrangeStringPattern2 :: SimpleFormative -> String
arrangeStringPattern2 (SimpleFormative {..} )
    = let (c1,c2) = consonantChunks SimpleFormative{_pattern',_sdt',_vowel1',_vowel2',_root'} 
    -- in let strSDT = convertSDT_To_String SimpleFormative{_sdt'} 
            in [i|#{convertSDT_To_String _sdt'}#{_vowel1'}#{c1}#{c2}#{_vowel2'}|]

arrangeStringPattern3 :: SimpleFormative -> String
arrangeStringPattern3 (SimpleFormative {..} )
    = let (c1,c2) = consonantChunks SimpleFormative{_pattern',_sdt',_vowel1',_vowel2',_root'} 
    -- in let strSDT = convertSDT_To_String SimpleFormative{_sdt'} 
            in [i|#{convertSDT_To_String _sdt'}#{c1}#{c2}#{_vowel1'}#{_vowel2'}|]

-- convertSimpleFormativeToString :: SimpleFormative -> String
-- convertSimpleFormativeToString SimpleFormative{_pattern' = pattern}
--     | pattern == Pat1 = "Debug 1" 
--     | pattern == Pat2 = "Debug2"
--     | otherwise  = "Debug 3"

convertSimpleFormativeToString :: SimpleFormative -> String
-- Suuuuper inelegant....
convertSimpleFormativeToString SimpleFormative{_pattern',_sdt',_vowel1',_vowel2',_root'}
    | _pattern' == Pat1 = arrangeStringPattern1 SimpleFormative{_pattern',_sdt',_vowel1',_vowel2',_root'}
    | _pattern' == Pat2 = arrangeStringPattern2 SimpleFormative{_pattern',_sdt',_vowel1',_vowel2',_root'}
    | otherwise  = arrangeStringPattern3 SimpleFormative{_pattern',_sdt',_vowel1',_vowel2',_root'}

convertFormativeToString :: Formative -> String
{-
-- Even more Inelegant
-- convertFormativeToString Formative{_pattern = ptrn, _sdt = sdt, _vowel1 = v1, _vowel2 = v2, _root = rt}
-}
convertFormativeToString (Formative{..})
    = let frm = Formative{_valence, _pattern, _sdt, _vowel1, _vowel2, _root, _structure, _perspective, _extension, _variance, _fuzziness, _affiliation}
    in [i|#{convFrm2SmplStrn frm}#{convertPSE_To_String $ grabPSE frm}#{convertVFA_To_String $ grabVFA frm}|]


convFrm2SmplStrn :: Formative -> String
convFrm2SmplStrn frm = convertSimpleFormativeToString $ grabCore frm

display :: String -> IO()
display s = putStrLn $ ( s ++ "\n")

totalDisplay :: Formative -> IO()
totalDisplay frm = display $ convertFormativeToString frm

-- data Card = Card
--   { _name        :: String
--   , _level       :: Int
--   , _description :: String
--   , _attack      :: Int
--   , _defense     :: Int
--   }
--   deriving (Eq, Show)

-- blueEyesWhiteDragon :: Card
-- blueEyesWhiteDragon = Card
--   { _name        = "Blue Eyes White Dragon"
--   , _level       = 8
--   , _description = "This legendary dragon is a powerful engine of destruction. Virtually invincible, very few have faced this awesome creature and lived to tell the tale."
--   , _attack      = 3000
--   , _defense     = 2500
--   }

-- display :: Card -> String
-- display (Card {..}) =
--   [i|#{_name} Lv.#{_level}) [#{_attack}/#{_defense}]|]
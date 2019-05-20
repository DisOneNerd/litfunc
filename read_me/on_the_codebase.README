# On The Codebase

Currently the codebase is monolithic.

It is also not very user friendly.
I hope to change this with time.

All the main code of the language is currently in Semantemes.hs

## Installation

Use the package manager [pip](https://pip.pypa.io/en/stable/) to install foobar.

```bash
pip install foobar
```

## Usage

Currently, the code only will lightly aide someone in making basic formatives. The values for the data constructors used in the formative appear in the codebase.
A formative currently is coded to consist of the following:

```hs
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
```

Currently, to build a formative, try editing the bottom of Semantemes.hs to create a new instance of a Formative data type. For example:

```hs
formativeName :: Formative
formativeName = Formative{
    _valence = MNO,
    _pattern = Pat1,
    _sdt = Tool,
    _vowel1 = "á",
    _vowel2 = "a",
    _root = "mfsk",

    _perspective = Unbounded,
    _structure = Discrete,
    _extension = Delimitive,

    _variance = Heterogenous,
    _fuzziness = Distinct,
    _affiliation = NoSignificance
}
```

Once the formative is defined, then one can run ``totalDisplay`` function to see what the formative would be transcribed as in the language.

```hs
*Main> totalDisplay medicine
twimfáskalasi
```
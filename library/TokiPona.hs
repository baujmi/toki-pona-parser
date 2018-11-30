module TokiPona where

import qualified Data.Text as Text
import Data.Text (Text)
import Text.Trifecta
import Control.Applicative
import qualified Text.Show.Prettyprint as PP

data S
  = S0 Interjection
  | S1 VocativeS
  | S2 Sent
  | S3 YNAnswer
  deriving (Eq, Read, Show)

data VocativeS
  = VocativeS0 NP
  | VocativeS1 NP
               Sent
  | VocativeS2 NP
               Pred
  | VocativeS3 Pred
  | VocativeS4 Conditional
               Pred
  deriving (Eq, Read, Show)

data YNAnswer
  = YNAnswerV
  | YNAnswerVala
  deriving (Eq, Read, Show)

data Sent
  = Sent0 SubjPred
  | Sent1 Conditional
          SubjPred
  | Sent2 SubjPred
  deriving (Eq, Read, Show)

data Conditional
  = Conditional0 SubjPred
  | Conditional1 Context
  | Conditional2 NP
  deriving (Eq, Read, Show)

data SubjPred
  = SubjPred0 Pred
  | SubjPred1 Pred
  | SubjPred2 NP_NoMiSina
              Pred
  | SubjPred3 CompoundSubj
              Pred
  deriving (Eq, Read, Show)

data CompoundSubj =
  CompoundSubj NP
               CompoundSubj
  deriving (Eq, Read, Show)

data NP_NoMiSina
  = NP_NoMiSina0 N_NoMiSina
  | NP_NoMiSina1 CompNP
  deriving (Eq, Read, Show)

data NP
  = NP0 N
  | NP1 CompNP
  deriving (Eq, Read, Show)

data CompNP
  = CompNP0 NMod
  | CompNP1 NPpi
  | CompNP2 NP
            NP
  deriving (Eq, Read, Show)

data NPpi
  = NPpi0 NP
          N
          Modifier
  | NPpi1 NP
          Name
  deriving (Eq, Read, Show)

data NMod
  = NMod0 N
          Modifier
  | NMod1 N
          N
  | NMod2 N
          N
          Modifier
  deriving (Eq, Read, Show)

data Modifier
  = Modifier0 Mod
  | Modifier1 Mod
              Modifier
  deriving (Eq, Read, Show)

data Pred
  = Pred0 VP
  | Pred1 VP
          Pred
  deriving (Eq, Read, Show)

data VP
  = VP0 IntransVP
  | VP1 TransVP
  | VP2 VP
        PrepPh
  deriving (Eq, Read, Show)

data IntransVP
  = IntransVP0 Verb
  | IntransVP1 NP
  | IntransVP2 NP
  | IntransVP3 Modal
               NP
  | IntransVP4 Modal
               NP
  | IntransVP5 Modifier
  | IntransVP6 NP
  deriving (Eq, Read, Show)

data TransVP
  = TransVP0 Verb
             DO
  | TransVP1 Modal
             Verb
             DO
  deriving (Eq, Read, Show)

data DO
  = DO0 NP
  | DO1 NP
        DO
  deriving (Eq, Read, Show)

data Verb
  = Verb0 V
  | Verb1 Modal
          V
  | Verb2 V
          Mod
  | Verb3 YnV
  | Verb4 Modal
          YnV
  | Verb5 YnV
          Mod
  deriving (Eq, Read, Show)

data YnV =
  YnV V
      V
  deriving (Eq, Read, Show)

data PrepPh
  = PrepPh0 Prep
            NP
  | PrepPh1 Prep
            NP
            PrepPh
  deriving (Eq, Read, Show)

data Context
  = ContextAnte
  | ContextKen
  deriving (Eq, Read, Show)

data Modal
  = Modal0 PosModal
  | Modal1 PosModal
  | Modal2 YnModal
  deriving (Eq, Read, Show)

data PosModal
  = PosModalKama
  | PosModalKen
  | PosModalWile
  deriving (Eq, Read, Show)

data YnModal
  = YnModalKamaAlaKama
  | YnModalKenAlaKen
  | YnModalWileAlaWile
  deriving (Eq, Read, Show)

data V
  = VAlasa
  | VAnpa
  | VAnte
  | VAwen
  | VEsun
  | VIjo
  | VIke
  | VJaki
  | VJan
  | VJo
  | VKalama
  | VKama
  | VKen
  | VKepeken
  | VKule
  | VLape
  | VLawa
  | VLete
  | VLili
  | VLon
  | VLukin
  | VMoku
  | VMoli
  | VMusi
  | VMute
  | VNasa
  | VOlin
  | VOpen
  | VPakala
  | VPali
  | VPana
  | VPilin
  | VPimeja
  | VPini
  | VPoka
  | VPona
  | VSeli
  | VSin
  | VSitelen
  | VSona
  | VSuli
  | VSuwi
  | VTawa
  | VTelo
  | VToki
  | VTomo
  | VTu
  | VUnpa
  | VUtala
  | VWan
  | VWawa
  | VWeka
  | VWile
  deriving (Eq, Read, Show)

data N
  = NMi
  | NSina
  | NN_NoMiSina
  deriving (Eq, Read, Show)

data N_NoMiSina
  = N_NoMiSinaName Name
  | N_NoMiSinaAkesi
  | N_NoMiSinaAla
  | N_NoMiSinaAle
  | N_NoMiSinaAli
  | N_NoMiSinaAnte
  | N_NoMiSinaEsun
  | N_NoMiSinaIjo
  | N_NoMiSinaIke
  | N_NoMiSinaIlo
  | N_NoMiSinaInsa
  | N_NoMiSinaJaki
  | N_NoMiSinaJan
  | N_NoMiSinaJo
  | N_NoMiSinaKala
  | N_NoMiSinaKalama
  | N_NoMiSinaKama
  | N_NoMiSinaKasi
  | N_NoMiSinaKen
  | N_NoMiSinaKili
  | N_NoMiSinaKiwen
  | N_NoMiSinaKule
  | N_NoMiSinaKute
  | N_NoMiSinaKulupu
  | N_NoMiSinaLawa
  | N_NoMiSinaLen
  | N_NoMiSinaLete
  | N_NoMiSinaLili
  | N_NoMiSinaLinja
  | N_NoMiSinaLipu
  | N_NoMiSinaLuka
  | N_NoMiSinaLupa
  | N_NoMiSinaMa
  | N_NoMiSinaMama
  | N_NoMiSinaMani
  | N_NoMiSinaMeli
  | N_NoMiSinaMije
  | N_NoMiSinaMoku
  | N_NoMiSinaMoli
  | N_NoMiSinaMonsi
  | N_NoMiSinaMun
  | N_NoMiSinaMusi
  | N_NoMiSinaMute
  | N_NoMiSinaNanpa
  | N_NoMiSinaNasin
  | N_NoMiSinaNena
  | N_NoMiSinaNi
  | N_NoMiSinaNimi
  | N_NoMiSinaNoka
  | N_NoMiSinaOko
  | N_NoMiSinaOlin
  | N_NoMiSinaOna
  | N_NoMiSinaPakala
  | N_NoMiSinaPali
  | N_NoMiSinaPalisa
  | N_NoMiSinaPan
  | N_NoMiSinaPana
  | N_NoMiSinaPilin
  | N_NoMiSinaPimeja
  | N_NoMiSinaPini
  | N_NoMiSinaPipi
  | N_NoMiSinaPoki
  | N_NoMiSinaPoka
  | N_NoMiSinaPona
  | N_NoMiSinaSeli
  | N_NoMiSinaSelo
  | N_NoMiSinaSeme
  | N_NoMiSinaSewi
  | N_NoMiSinaSijelo
  | N_NoMiSinaSike
  | N_NoMiSinaSinpin
  | N_NoMiSinaSitelen
  | N_NoMiSinaSona
  | N_NoMiSinaSoweli
  | N_NoMiSinaSuli
  | N_NoMiSinaSuno
  | N_NoMiSinaSupa
  | N_NoMiSinaSuwi
  | N_NoMiSinaTan
  | N_NoMiSinaTawa
  | N_NoMiSinaTelo
  | N_NoMiSinaTenpo
  | N_NoMiSinaToki
  | N_NoMiSinaTomo
  | N_NoMiSinaTu
  | N_NoMiSinaUnpa
  | N_NoMiSinaUta
  | N_NoMiSinaUtala
  | N_NoMiSinaWalo
  | N_NoMiSinaWan
  | N_NoMiSinaWaso
  | N_NoMiSinaWawa
  | N_NoMiSinaWeka
  | N_NoMiSinaWile
  deriving (Eq, Read, Show)

data Mod
  = ModAla
  | ModAle
  | ModAli
  | ModAnte
  | ModAwen
  | ModIjo
  | ModIke
  | ModInsa
  | ModJaki
  | ModJan
  | ModJelo
  | ModKalama
  | ModKama
  | ModKin
  | ModKiwen
  | ModKule
  | ModKute
  | ModKulupu
  | ModLaso
  | ModLape
  | ModLawa
  | ModLete
  | ModLili
  | ModLoje
  | ModLukin
  | ModMama
  | ModMeli
  | ModMi
  | ModMije
  | ModMoku
  | ModMoli
  | ModMonsi
  | ModMun
  | ModMusi
  | ModMute
  | ModNasa
  | ModNi
  | ModOlin
  | ModOna
  | ModPakala
  | ModPali
  | ModPimeja
  | ModPini
  | ModPoka
  | ModPona
  | ModPu
  | ModSama
  | ModSeli
  | ModSeme
  | ModSewi
  | ModSike
  | ModSin
  | ModSina
  | ModSuli
  | ModSuwi
  | ModTaso
  | ModTawa
  | ModTelo
  | ModToki
  | ModTomo
  | ModTu
  | ModUnpa
  | ModUta
  | ModWalo
  | ModWan
  | ModWawa
  | ModWeka
  | ModWile
  deriving (Eq, Read, Show)

data Prep
  = PrepKepeken
  | PrepLon
  | PrepPoka
  | PrepSama
  | PrepTan
  | PrepTawa
  deriving (Eq, Read, Show)

data Interjection
  = InterjectionA
  | InterjectionAA
  | InterjectionAAA
  | InterjectionAla
  | InterjectionIke
  | InterjectionJaki
  | InterjectionMu
  | InterjectionO
  | InterjectionPakala
  | InterjectionPona
  | InterjectionToki
  deriving (Eq, Read, Show)

data Name
  = NameJan String
  | NameMa String
  | NameMaTomo String
  | NameToki String
  | NameSoweli String
  | NamePan String
  deriving (Eq, Read, Show)


word :: String -> Parser String
word s = token (string s)



pName :: Parser Name
pName =
  (word "jan" *> fmap NameJan name) <|>
  (word "ma" *> ((word "tomo" *> fmap NameMaTomo name) <|> fmap NameMa name)) <|>
  (word "toki" *> fmap NameToki name) <|>
  (word "soweli" *> fmap NameSoweli name) <|>
  (word "pan" *> fmap NamePan name)
  where
    name :: Parser String
    name =
      try
        (do firstLetterOfName <- upper
            restOfName <- many alphaNum
            return (firstLetterOfName : restOfName)) <?>
      "Name"


testParser :: Show a => Parser a -> String -> IO ()
testParser p s =
  case runParser p mempty s of
    Success x -> PP.prettyPrint x
    Failure y -> print (_errDoc y)

main :: IO ()
main = putStrLn "pona"

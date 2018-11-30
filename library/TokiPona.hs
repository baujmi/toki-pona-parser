module TokiPona where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Maybe
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Text.Show.Prettyprint as PP
import Text.Trifecta

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

pNP = undefined

data CompNP
  = CompNP0 NMod
  | CompNP1 NPpi
  | CompNP2 NP
            NP
  deriving (Eq, Read, Show)

data NPpi
  = NPpiNounModifier NP
          N
          Modifier
  | NPpiName NP
          Name
  deriving (Eq, Read, Show)

-- TODO: here

data NMod =
  NMod N
       (Maybe N)
       (Maybe Modifier)
  deriving (Eq, Read, Show)

pNMod :: Parser NMod
pNMod =
  try
    (do n0 <- pN
        n1 <- fmap Just pN <|> pure Nothing
        modifier <- fmap Just pModifier <|> pure Nothing
        -- NMod may have n1, modifier, or both, but it *must have at least one*
        guard (isJust n1 || isJust modifier)
        pure (NMod n0 n1 modifier))

data Modifier
  = Modifier Mod (Maybe Modifier)
  deriving (Eq, Read, Show)

pModifier :: Parser Modifier
pModifier = try (liftA2 Modifier pMod (fmap Just pModifier <|> pure Nothing))

data Pred
  = Pred VP (Maybe Pred)
  deriving (Eq, Read, Show)

pPred :: Parser Pred
pPred = try (liftA2 Pred pVP (fmap Just pPred <|> pure Nothing))

data VP
  = VPIntrans IntransVP
  | VPTrans TransVP
  | VPPrep VP
        PrepPh
  deriving (Eq, Read, Show)

pVP :: Parser VP
pVP = try (fmap VPTrans pTransVP <|> fmap VPIntrans pIntransVP <|> liftA2 VPPrep pVP pPrepPh)

data IntransVP
  = IntransVP Verb
  | IntransVPLon NP
  | IntransVPTawa NP
  | IntransVPLonModal Modal
               NP
  | IntransVPTawaModal Modal
               NP
  | IntransVPModifier Modifier
  | IntransVPNP NP
  deriving (Eq, Read, Show)

-- IntransVP -> Verb | lon NP | tawa NP | Modal lon NP | Modal tawa NP | Modifier | NP
pIntransVP :: Parser IntransVP
pIntransVP = try (verbLon <|> verbTawa <|> verbBasic <|> fmap IntransVPModifier pModifier <|> fmap IntransVPNP pNP)
  where
    verbBasic = fmap IntransVP pVerb
    verbLon = do
      _ <- word "lon"
      np <- pNP
      fmap (IntransVPLonModal np) pModal <|> pure (IntransVPLon np)
    verbTawa = do
      _ <- word "tawa"
      np <- pNP
      fmap (IntransVPTawaModal np) pModal <|> pure (IntransVPTawa np)
data TransVP
  = TransVP (Maybe Modal)
            Verb
            DO
  deriving (Eq, Read, Show)

pTransVP :: Parser TransVP
pTransVP =
  try (liftA3 TransVP (fmap Just pModal <|> pure Nothing) pVerb pDO)

data DO
  = DO NP (Maybe DO)
  deriving (Eq, Read, Show)

pDO :: Parser DO
pDO =
  try (word "e" *> liftA2 DO pNP (fmap Just pDO <|> pure Nothing))

data Verb
  = Verb V
  | VerbModal Modal
              V
  | VerbModalQuestion Modal
                      YnV
  | VerbMod V
            Mod
  | VerbModQuestion YnV
                    Mod
  | VerbQuestion YnV
  deriving (Eq, Read, Show)

pVerb :: Parser Verb
-- V            olin
  -- V Mod      olin sin
-- Modal        kama ala
  -- Modal V    ken toki
  -- Modal YnV  ken ken ala ken
-- YnV          ike ala ike
  -- YnV Mod    ike ala ike ala
pVerb = try (verbQuestion <|> verbModal <|> verbBasic)
  where
    verbBasic = do
      v <- pV
      fmap (VerbMod v) pMod <|> pure (Verb v)
    verbModal = do
      m <- pModal
      fmap (VerbModalQuestion m) pYnV <|> fmap (VerbModal m) pV
    verbQuestion = do
      q <- pYnV
      fmap (VerbModQuestion q) pMod <|> pure (VerbQuestion q)

data YnV =
  YnV V
      V
  deriving (Eq, Read, Show)

pYnV :: Parser YnV
pYnV = try (do
  ko'a <- pV
  _ <- word "ala"
  ko'e <- pV
  guard (ko'a == ko'e)
  pure (YnV ko'a ko'e))

data PrepPh =
  PrepPh Prep
         NP
         (Maybe PrepPh)
  deriving (Eq, Read, Show)

pPrepPh :: Parser PrepPh
pPrepPh = try (liftA3 PrepPh pPrep pNP (fmap Just pPrepPh <|> pure Nothing))

data Context
  = ContextAnte
  | ContextKen
  deriving (Eq, Read, Show)

pContext :: Parser Context
pContext = try ((word "ante" *> pure ContextAnte) <|> (word "ken" *> pure ContextKen))

data Modal
  = Modal0 PosModal
  | Modal1 PosModal
  | Modal2 YnModal
  deriving (Eq, Read, Show)

pModal :: Parser Modal
pModal =
  try
    ((do modal <- pPosModal
         (word "ala" *> pure (Modal1 modal)) <|> pure (Modal0 modal)) <|>
     fmap Modal2 pYnModal)

data PosModal
  = PosModalKama
  | PosModalKen
  | PosModalWile
  deriving (Eq, Read, Show)

pPosModal :: Parser PosModal
pPosModal =
  try ((word "kama" *> pure PosModalKama) <|>
       (word "ken" *> pure PosModalKen) <|>
       (word "wile" *> pure PosModalWile))

data YnModal
  = YnModalKamaAlaKama
  | YnModalKenAlaKen
  | YnModalWileAlaWile
  deriving (Eq, Read, Show)

pYnModal :: Parser YnModal
pYnModal =
  try ((word "kama" *> word "ala" *> word "kama" *> pure YnModalKamaAlaKama) <|>
  (word "ken" *> word "ala" *> word "ken" *> pure YnModalKenAlaKen) <|>
  (word "wile" *> word "ala" *> word "wile" *> pure YnModalWileAlaWile))

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

pV :: Parser V
pV =
  try ((word "alasa" *> pure VAlasa) <|>
   (word "anpa" *> pure VAnpa) <|>
   (word "ante" *> pure VAnte) <|>
   (word "awen" *> pure VAwen) <|>
   (word "esun" *> pure VEsun) <|>
   (word "ijo" *> pure VIjo) <|>
   (word "ike" *> pure VIke) <|>
   (word "jaki" *> pure VJaki) <|>
   (word "jan" *> pure VJan) <|>
   (word "jo" *> pure VJo) <|>
   (word "kalama" *> pure VKalama) <|>
   (word "kama" *> pure VKama) <|>
   (word "ken" *> pure VKen) <|>
   (word "kepeken" *> pure VKepeken) <|>
   (word "kule" *> pure VKule) <|>
   (word "lape" *> pure VLape) <|>
   (word "lawa" *> pure VLawa) <|>
   (word "lete" *> pure VLete) <|>
   (word "lili" *> pure VLili) <|>
   (word "lon" *> pure VLon) <|>
   (word "lukin" *> pure VLukin) <|>
   (word "moku" *> pure VMoku) <|>
   (word "moli" *> pure VMoli) <|>
   (word "musi" *> pure VMusi) <|>
   (word "mute" *> pure VMute) <|>
   (word "nasa" *> pure VNasa) <|>
   (word "olin" *> pure VOlin) <|>
   (word "open" *> pure VOpen) <|>
   (word "pakala" *> pure VPakala) <|>
   (word "pali" *> pure VPali) <|>
   (word "pana" *> pure VPana) <|>
   (word "pilin" *> pure VPilin) <|>
   (word "pimeja" *> pure VPimeja) <|>
   (word "pini" *> pure VPini) <|>
   (word "poka" *> pure VPoka) <|>
   (word "pona" *> pure VPona) <|>
   (word "seli" *> pure VSeli) <|>
   (word "sin" *> pure VSin) <|>
   (word "sitelen" *> pure VSitelen) <|>
   (word "sona" *> pure VSona) <|>
   (word "suli" *> pure VSuli) <|>
   (word "suwi" *> pure VSuwi) <|>
   (word "tawa" *> pure VTawa) <|>
   (word "telo" *> pure VTelo) <|>
   (word "toki" *> pure VToki) <|>
   (word "tomo" *> pure VTomo) <|>
   (word "tu" *> pure VTu) <|>
   (word "unpa" *> pure VUnpa) <|>
   (word "utala" *> pure VUtala) <|>
   (word "wan" *> pure VWan) <|>
   (word "wawa" *> pure VWawa) <|>
   (word "weka" *> pure VWeka) <|>
   (word "wile" *> pure VWile)) <?>
  "Verb (transitive or intransitive)"

data N
  = NMi
  | NSina
  | NN_NoMiSina N_NoMiSina
  deriving (Eq, Read, Show)

pN :: Parser N
pN =
  try ((word "mi" *> pure NMi) <|> (word "sina" *> pure NSina) <|> fmap NN_NoMiSina pN_NoMiSina)

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

pN_NoMiSina :: Parser N_NoMiSina
pN_NoMiSina =
  try (((fmap N_NoMiSinaName pName)) <|> (word "akesi" *> pure N_NoMiSinaAkesi) <|>
   (word "ala" *> pure N_NoMiSinaAla) <|>
   (word "ale" *> pure N_NoMiSinaAle) <|>
   (word "ali" *> pure N_NoMiSinaAli) <|>
   (word "ante" *> pure N_NoMiSinaAnte) <|>
   (word "esun" *> pure N_NoMiSinaEsun) <|>
   (word "ijo" *> pure N_NoMiSinaIjo) <|>
   (word "ike" *> pure N_NoMiSinaIke) <|>
   (word "ilo" *> pure N_NoMiSinaIlo) <|>
   (word "insa" *> pure N_NoMiSinaInsa) <|>
   (word "jaki" *> pure N_NoMiSinaJaki) <|>
   (word "jan" *> pure N_NoMiSinaJan) <|>
   (word "jo" *> pure N_NoMiSinaJo) <|>
   (word "kala" *> pure N_NoMiSinaKala) <|>
   (word "kalama" *> pure N_NoMiSinaKalama) <|>
   (word "kama" *> pure N_NoMiSinaKama) <|>
   (word "kasi" *> pure N_NoMiSinaKasi) <|>
   (word "ken" *> pure N_NoMiSinaKen) <|>
   (word "kili" *> pure N_NoMiSinaKili) <|>
   (word "kiwen" *> pure N_NoMiSinaKiwen) <|>
   (word "kule" *> pure N_NoMiSinaKule) <|>
   (word "kute" *> pure N_NoMiSinaKute) <|>
   (word "kulupu" *> pure N_NoMiSinaKulupu) <|>
   (word "lawa" *> pure N_NoMiSinaLawa) <|>
   (word "len" *> pure N_NoMiSinaLen) <|>
   (word "lete" *> pure N_NoMiSinaLete) <|>
   (word "lili" *> pure N_NoMiSinaLili) <|>
   (word "linja" *> pure N_NoMiSinaLinja) <|>
   (word "lipu" *> pure N_NoMiSinaLipu) <|>
   (word "luka" *> pure N_NoMiSinaLuka) <|>
   (word "lupa" *> pure N_NoMiSinaLupa) <|>
   (word "ma" *> pure N_NoMiSinaMa) <|>
   (word "mama" *> pure N_NoMiSinaMama) <|>
   (word "mani" *> pure N_NoMiSinaMani) <|>
   (word "meli" *> pure N_NoMiSinaMeli) <|>
   (word "mije" *> pure N_NoMiSinaMije) <|>
   (word "moku" *> pure N_NoMiSinaMoku) <|>
   (word "moli" *> pure N_NoMiSinaMoli) <|>
   (word "monsi" *> pure N_NoMiSinaMonsi) <|>
   (word "mun" *> pure N_NoMiSinaMun) <|>
   (word "musi" *> pure N_NoMiSinaMusi) <|>
   (word "mute" *> pure N_NoMiSinaMute) <|>
   (word "nanpa" *> pure N_NoMiSinaNanpa) <|>
   (word "nasin" *> pure N_NoMiSinaNasin) <|>
   (word "nena" *> pure N_NoMiSinaNena) <|>
   (word "ni" *> pure N_NoMiSinaNi) <|>
   (word "nimi" *> pure N_NoMiSinaNimi) <|>
   (word "noka" *> pure N_NoMiSinaNoka) <|>
   (word "oko" *> pure N_NoMiSinaOko) <|>
   (word "olin" *> pure N_NoMiSinaOlin) <|>
   (word "ona" *> pure N_NoMiSinaOna) <|>
   (word "pakala" *> pure N_NoMiSinaPakala) <|>
   (word "pali" *> pure N_NoMiSinaPali) <|>
   (word "palisa" *> pure N_NoMiSinaPalisa) <|>
   (word "pan" *> pure N_NoMiSinaPan) <|>
   (word "pana" *> pure N_NoMiSinaPana) <|>
   (word "pilin" *> pure N_NoMiSinaPilin) <|>
   (word "pimeja" *> pure N_NoMiSinaPimeja) <|>
   (word "pini" *> pure N_NoMiSinaPini) <|>
   (word "pipi" *> pure N_NoMiSinaPipi) <|>
   (word "poki" *> pure N_NoMiSinaPoki) <|>
   (word "poka" *> pure N_NoMiSinaPoka) <|>
   (word "pona" *> pure N_NoMiSinaPona) <|>
   (word "seli" *> pure N_NoMiSinaSeli) <|>
   (word "selo" *> pure N_NoMiSinaSelo) <|>
   (word "seme" *> pure N_NoMiSinaSeme) <|>
   (word "sewi" *> pure N_NoMiSinaSewi) <|>
   (word "sijelo" *> pure N_NoMiSinaSijelo) <|>
   (word "sike" *> pure N_NoMiSinaSike) <|>
   (word "sinpin" *> pure N_NoMiSinaSinpin) <|>
   (word "sitelen" *> pure N_NoMiSinaSitelen) <|>
   (word "sona" *> pure N_NoMiSinaSona) <|>
   (word "soweli" *> pure N_NoMiSinaSoweli) <|>
   (word "suli" *> pure N_NoMiSinaSuli) <|>
   (word "suno" *> pure N_NoMiSinaSuno) <|>
   (word "supa" *> pure N_NoMiSinaSupa) <|>
   (word "suwi" *> pure N_NoMiSinaSuwi) <|>
   (word "tan" *> pure N_NoMiSinaTan) <|>
   (word "tawa" *> pure N_NoMiSinaTawa) <|>
   (word "telo" *> pure N_NoMiSinaTelo) <|>
   (word "tenpo" *> pure N_NoMiSinaTenpo) <|>
   (word "toki" *> pure N_NoMiSinaToki) <|>
   (word "tomo" *> pure N_NoMiSinaTomo) <|>
   (word "tu" *> pure N_NoMiSinaTu) <|>
   (word "unpa" *> pure N_NoMiSinaUnpa) <|>
   (word "uta" *> pure N_NoMiSinaUta) <|>
   (word "utala" *> pure N_NoMiSinaUtala) <|>
   (word "walo" *> pure N_NoMiSinaWalo) <|>
   (word "wan" *> pure N_NoMiSinaWan) <|>
   (word "waso" *> pure N_NoMiSinaWaso) <|>
   (word "wawa" *> pure N_NoMiSinaWawa) <|>
   (word "weka" *> pure N_NoMiSinaWeka) <|>
   (word "wile" *> pure N_NoMiSinaWile)) <?>
  "Noun (other than mi or sina)"

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

pMod :: Parser Mod
pMod =
  ((word "ala" *> pure ModAla) <|>
   (word "ale" *> pure ModAle) <|>
   (word "ali" *> pure ModAli) <|>
   (word "ante" *> pure ModAnte) <|>
   (word "awen" *> pure ModAwen) <|>
   (word "ijo" *> pure ModIjo) <|>
   (word "ike" *> pure ModIke) <|>
   (word "insa" *> pure ModInsa) <|>
   (word "jaki" *> pure ModJaki) <|>
   (word "jan" *> pure ModJan) <|>
   (word "jelo" *> pure ModJelo) <|>
   (word "kalama" *> pure ModKalama) <|>
   (word "kama" *> pure ModKama) <|>
   (word "kin" *> pure ModKin) <|>
   (word "kiwen" *> pure ModKiwen) <|>
   (word "kule" *> pure ModKule) <|>
   (word "kute" *> pure ModKute) <|>
   (word "kulupu" *> pure ModKulupu) <|>
   (word "laso" *> pure ModLaso) <|>
   (word "lape" *> pure ModLape) <|>
   (word "lawa" *> pure ModLawa) <|>
   (word "lete" *> pure ModLete) <|>
   (word "lili" *> pure ModLili) <|>
   (word "loje" *> pure ModLoje) <|>
   (word "lukin" *> pure ModLukin) <|>
   (word "mama" *> pure ModMama) <|>
   (word "meli" *> pure ModMeli) <|>
   (word "mi" *> pure ModMi) <|>
   (word "mije" *> pure ModMije) <|>
   (word "moku" *> pure ModMoku) <|>
   (word "moli" *> pure ModMoli) <|>
   (word "monsi" *> pure ModMonsi) <|>
   (word "mun" *> pure ModMun) <|>
   (word "musi" *> pure ModMusi) <|>
   (word "mute" *> pure ModMute) <|>
   (word "nasa" *> pure ModNasa) <|>
   (word "ni" *> pure ModNi) <|>
   (word "olin" *> pure ModOlin) <|>
   (word "ona" *> pure ModOna) <|>
   (word "pakala" *> pure ModPakala) <|>
   (word "pali" *> pure ModPali) <|>
   (word "pimeja" *> pure ModPimeja) <|>
   (word "pini" *> pure ModPini) <|>
   (word "poka" *> pure ModPoka) <|>
   (word "pona" *> pure ModPona) <|>
   (word "pu" *> pure ModPu) <|>
   (word "sama" *> pure ModSama) <|>
   (word "seli" *> pure ModSeli) <|>
   (word "seme" *> pure ModSeme) <|>
   (word "sewi" *> pure ModSewi) <|>
   (word "sike" *> pure ModSike) <|>
   (word "sin" *> pure ModSin) <|>
   (word "sina" *> pure ModSina) <|>
   (word "suli" *> pure ModSuli) <|>
   (word "suwi" *> pure ModSuwi) <|>
   (word "taso" *> pure ModTaso) <|>
   (word "tawa" *> pure ModTawa) <|>
   (word "telo" *> pure ModTelo) <|>
   (word "toki" *> pure ModToki) <|>
   (word "tomo" *> pure ModTomo) <|>
   (word "tu" *> pure ModTu) <|>
   (word "unpa" *> pure ModUnpa) <|>
   (word "uta" *> pure ModUta) <|>
   (word "walo" *> pure ModWalo) <|>
   (word "wan" *> pure ModWan) <|>
   (word "wawa" *> pure ModWawa) <|>
   (word "weka" *> pure ModWeka) <|>
   (word "wile" *> pure ModWile)) <?>
  "Modifier (English: adjective)"

data Prep
  = PrepKepeken
  | PrepLon
  | PrepPoka
  | PrepSama
  | PrepTan
  | PrepTawa
  deriving (Eq, Read, Show)

pPrep :: Parser Prep
pPrep =
  ((word "kepeken" *> pure PrepKepeken) <|>
   (word "lon" *> pure PrepLon) <|>
   (word "poka" *> pure PrepPoka) <|>
   (word "sama" *> pure PrepSama) <|>
   (word "tan" *> pure PrepTan) <|>
   (word "tawa" *> pure PrepTawa)) <?>
  "Preposition (English: https://www.grammarly.com/blog/prepositions/)"

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

pInterjection :: Parser Interjection
pInterjection =
  (word "a" *>
   ((word "a" *> ((word "a" *> pure InterjectionAAA) <|> pure InterjectionAA)) <|>
    pure InterjectionA)) <|>
  (word "ala" *> pure InterjectionAla) <|>
  (word "ike" *> pure InterjectionIke) <|>
  (word "jaki" *> pure InterjectionJaki) <|>
  (word "mu" *> pure InterjectionMu) <|>
  (word "o" *> pure InterjectionO) <|>
  (word "pakala" *> pure InterjectionPakala) <|>
  (word "pona" *> pure InterjectionPona) <|>
  (word "toki" *> pure InterjectionIke)

data Name
  = NameJan String
  | NameMa String
  | NameMaTomo String
  | NameToki String
  | NameSoweli String
  | NamePan String
  deriving (Eq, Read, Show)

word :: String -> Parser String
word s = try (token (string s <* (void (satisfy (not . isAlphaNum)) <|> eof)))

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
      "Name (Uppercase letter followed by letters and numbers)"

testParser :: Show a => Parser a -> String -> IO ()
testParser p s =
  case runParser p mempty s of
    Success x -> PP.prettyPrint x
    Failure y -> print (_errDoc y)

main :: IO ()
main = putStrLn "pona"

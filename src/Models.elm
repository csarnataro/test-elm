module Models exposing (Model, Work, Area(..), Lang(..))


type alias Model = 
  {
    currentWork: Work,
    latestWorks: List Work
  }
type Area 
  = Technical
  | Training
  | Management

type Lang 
  = Italian
  | English

type alias Work = 
  { 
    id : Int,
    language: Lang,
    area: Area,
    order: Int,
    title: String,
    description: String
  }


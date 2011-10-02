module Data.Data where
  
data Item = Constructor String
          | Type        String
          | Function    String
          | Operator    String
          | Class       String
  deriving (Eq, Ord)
  
instance Show Item where
  show (Constructor s) = s
  show (Type s)        = s
  show (Function s)    = s
  show (Operator s)    = s
  show (Class s)       = s
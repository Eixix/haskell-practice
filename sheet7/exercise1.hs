import Text.Read

data Person = Person {
  name   :: String,
  age    :: Int,
  emails :: String
} deriving Show

type Record = [(String, String)]

lookupPerson :: Record -> Maybe Person
lookupPerson rec =
  case lookup "name" rec of
    Nothing -> Nothing
    Just name -> case lookup "age" rec of
      Nothing -> Nothing
      Just age -> case lookup "email" rec of
        Nothing -> Nothing
        Just email -> Just $ Person name (read age) email

lookupPersonFunctor :: Record -> Maybe Person
lookupPersonFunctor rec = 
  Person
  <$> lookup "name" rec
  <*> (read <$> lookup "age" rec)
  <*> lookup "email" rec


lookupPersonMonad :: Record -> Maybe Person
lookupPersonMonad rec = 
  lookup "name" rec >>= \name ->
   read <$> lookup "age" rec >>= \age ->
      lookup "email" rec >>= \email -> 
        return $ Person name age email

lookupPersonMonadDo :: Record -> Maybe Person
lookupPersonMonadDo rec = do
  name <- lookup "name" rec
  age <- read <$> lookup "age" rec
  email <- lookup "email" rec
  return $ Person name age email

lookupPersonMonadDoWithRead :: Record -> Maybe Person
lookupPersonMonadDoWithRead rec = do
  name <- lookup "name" rec
  x <- lookup "age" rec
  age <- readMaybe x
  email <- lookup "email" rec
  return $ Person name age email

lookUpPersonSafe :: Record -> Either String Person
lookUpPersonSafe rec = do
  name <- case lookup "name" rec of
          Nothing -> Left "Person not there yet"
          Just x -> Right x
  email <- case lookup "email" rec of
           Nothing -> Left "Email not there yet"
           Just x -> Right x
  ageN <- case read <$> lookup "age" rec of
          Nothing -> Left "Wrong age format"
          Just x -> Right x
  ageS <- case ageN of
          Nothing -> Left "Age not there yet"
          Just x -> Right x
  return $ Person name ageS email

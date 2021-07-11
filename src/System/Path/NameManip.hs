{-# LANGUAGE Safe #-}
{- |
   Module     : System.Path.NameManip
   Copyright  : Copyright (C) 2004 Volker Wysk
   SPDX-License-Identifier: BSD-3-Clause

   Stability  : provisional
   Portability: portable

Low-level path name manipulations.

Written by Volker Wysk
-}

module System.Path.NameManip where

import           Data.List        (intercalate, unfoldr)
import           System.Directory (getCurrentDirectory)
import           System.FilePath  (isPathSeparator, pathSeparator, (</>))

{- | Split a path in components. Repeated \"@\/@\" characters don\'t lead to empty
components. \"@.@\" path components are removed. If the path is absolute, the first component
will start with \"@\/@\". \"@..@\" components are left intact. They can't be simply
removed, because the preceding component might be a symlink. In this case,
'realpath' is probably what you need.

The case that the path is empty, is probably an error. However, it is
treated like \"@.@\", yielding an empty path components list.

Examples:

>slicePath "/"        = ["/"]
>slicePath "/foo/bar" = ["/foo","bar"]
>slicePath "..//./"   = [".."]
>slicePath "."        = []

See 'unslicePath', 'realpath', 'realpath_s'.
-}
slicePath :: String    -- ^ The path to be broken to components.
           -> [String]  -- ^ List of path components.
slicePath "" = []
slicePath (c:cs) = if isPathSeparator c
                       then case slicePath' cs of
                           []     -> [[c]]
                           (p:ps) -> (c:p):ps
                       else slicePath' (c:cs)
    where
      slicePath' o = filter (\c' -> c' /= "" && c' /= ".") (split o)

      split xs = unfoldr f xs
        where
          f "" = Nothing
          f xs' = Just (tail' <$> break isPathSeparator xs')
          tail' [] = []
          tail' xs' = tail xs'

{- | Form a path from path components. This isn't the inverse
of 'slicePath', since @'unslicePath' . 'slicePath'@
normalises the path.

See 'slicePath'.
-}
unslicePath :: [String]        -- ^ List of path components
             -> String          -- ^ The path which consists of the supplied path components
unslicePath [] = "."
unslicePath cs = intercalate [pathSeparator] cs


{- | Normalise a path. This is done by reducing repeated @\/@ characters to one, and removing
@.@ path components. @..@ path components are left intact, because of possible symlinks.

@'normalisePath' = 'unslicePath' . 'slicePath'@
-}
normalisePath :: String        -- ^ Path to be normalised
               -> String        -- ^ Path in normalised form
normalisePath = unslicePath . slicePath


{- | Split a file name in components. This are the base file name and the
suffixes, which are separated by dots. If the name starts with a dot, it is
regarded as part of the base name. The result is a list of file name
components. The filename may be a path. In this case, everything up to the
last path component will be returned as part of the base file name. The
path gets normalised thereby.

No empty suffixes are returned. If the file name contains several
consecutive dots, they are regared as part of the preceding file name
component.

Concateneting the name components and adding dots, reproduces the
original name, with a normalised path:
@concat . intersperse \".\" . 'sliceFilename' == 'normalise'@.

Note that the last path component might be \"@..@\". Then it is not
possible to deduce the refered directory's name from the path. An IO
action for getting the real path is then necessary.

Examples:

@
'sliceFilename' \"a.b\/\/.\/.foo.tar.gz\" == [\"a.b\/.foo\",\"tar\",\"gz\"]
'sliceFilename' \".x..y.\"             == [\".x.\", \"y.\"]
@

See 'unsliceFilename', @sliceFilename\'@.
-}
sliceFilename :: String        -- ^ Path
               -> [String]      -- ^ List of components the file name is made up of
sliceFilename path =
  let comps = slicePath path
  in if null comps
        then []
        else -- sliceFilename' result not empty, because comps not empty
            let (base:suffixes) = sliceFilename' (last comps)
            in (unslicePath (init comps ++ [base]) : suffixes)


{- | This is a variant of 'sliceFilename'. It is like 'sliceFilename', except for
being more efficient, and the filename must not contain any preceding path,
since this case isn't considered.

See 'sliceFilename', 'unsliceFilename'.
-}
sliceFilename' :: String        -- ^ File name without path
                -> [String]      -- ^ List of components the file name is made up of
sliceFilename' filename =
  case filename of
    ('.':filename') -> case sliceFilename'' filename' of
                          []     -> ["."]
                          (t:ts) -> ('.':t) : ts
    filename' -> sliceFilename'' filename'
  where
    sliceFilename'' :: String -> [String]
    sliceFilename'' "" = []
    sliceFilename'' fn =
        let (beg,rest) = split1 fn
        in  (beg : sliceFilename'' rest)

    split1 :: String -> (String, String)
    split1 (x:y:r) =
        if x == '.' && y /= '.' then ("", y:r)
                                else let (beg,rest) = split1 (y:r)
                                    in  (x:beg,rest)
    split1 str = (str, "")

{- | Form file name from file name components, interspersing dots. This is
the inverse of 'sliceFilename', except for normalisation of any path.

> unsliceFilename = concat . intersperse "."

See 'sliceFilename'.
-}
unsliceFilename :: [String]    -- ^ List of file name components
                 -> String      -- ^ Name of the file which consists of the supplied components
unsliceFilename = intercalate "."


{- | Split a path in directory and file name. Only in the case that the
supplied path is empty, both parts are empty strings. Otherwise, @\".\"@ is filled in
for the corresponding part, if necessary. Unless the path is empty,
concatenating the returned path and file name components with a slash in
between, makes a valid path to the file.

@splitPath@ splits off the last path component. This
isn't the same as the text after the last @\/@.

Note that the last path component might be @\"..\"@. Then it is not
possible to deduce the refered directory's name from the path. Then an IO
action for getting the real path is necessary.

Examples:

>splitPath "/a/b/c"      == ("/a/b", "c")
>splitPath "foo"         == (".", "foo")
>splitPath "foo/bar"     == ("foo", "bar")
>splitPath "foo/.."      == ("foo", "..")
>splitPath "."           == (".", ".")
>splitPath ""            == ("", "")
>splitPath "/foo"        == ("/", "foo")
>splitPath "foo/"        == (".", "foo")
>splitPath "foo/."       == (".", "foo")
>splitPath "foo///./bar" == ("foo", "bar")

See 'slicePath'.
-}
splitPath :: String            -- ^ Path to be split
           -> (String, String)  -- ^ Directory and file name components of the path. The directory path is normalized.
splitPath "" = ("","")
splitPath path =
   case slicePath path of
      []     -> (".", ".")
      [""]   -> (".", "")
      [f:fs] -> if isPathSeparator f then ([pathSeparator], fs) else (".", f:fs)
      parts  -> ( unslicePath (init parts)
                , last parts
                )

{- | Get the directory part of a path.

>dirPart = fst . splitPath

See 'splitPath'.
-}
dirPart :: String -> String
dirPart = fst . splitPath


{- | Get the last path component of a path.

>filenamePart = snd . splitPath

Examples:

>filenamePart "foo/bar" == "bar"
>filenamePart "."       == "."

See 'splitPath'.
-}
filenamePart :: String -> String
filenamePart = snd . splitPath


{- | Inverse of 'splitPath', except for normalisation.

This concatenates two paths, and takes care of @\".\"@ and empty paths. When the two components are the result of @splitPath@, then @unsplitPath@
creates a normalised path. It is best documented by its definition:

>unsplitPath (".", "") = "."
>unsplitPath ("", ".") = "."
>unsplitPath (".", q)  = q
>unsplitPath ("", q)   = q
>unsplitPath (p, "")   = p
>unsplitPath (p, ".")  = p
>unsplitPath (p, q)    = p ++ "/" ++ q

Examples:

>unsplitPath ("", "")     == ""
>unsplitPath (".", "")    == "."
>unsplitPath (".", ".")   == "."
>unsplitPath ("foo", ".") == "foo"

See 'splitPath'.
-}
unsplitPath :: ( String, String )  -- ^ Directory and file name
             -> String          -- ^ Path formed from the directory and file name parts
unsplitPath (".", "") = "."
unsplitPath ("", ".") = "."
unsplitPath (".", q)  = q
unsplitPath ("", q)   = q
unsplitPath (p, "")   = p
unsplitPath (p, ".")  = p
unsplitPath (p, q)    = p </> q


{- | Split a file name in prefix and suffix. If there isn't any suffix in
the file name, then return an empty suffix. A dot at the beginning or at
the end is not regarded as introducing a suffix.

The last path component is what is being split. This isn't the same as
splitting the string at the last dot. For instance, if the file name
doesn't contain any dot, dots in previous path component's aren't mistaken
as introducing suffixes.

The path part is returned in normalised form. This means, @\".\"@ components
are removed, and multiple \"@\/@\"s are reduced to one.

Note that there isn't any plausibility check performed on the suffix. If the file name doesn't have a suffix, but happens to contain a dot, then this
dot is mistaken as introducing a suffix.

Examples:

>splitFilename "path/to/foo.bar"                             = ("path/to/foo","bar")
>splitFilename "path/to/foo"                                 = ("path/to/foo","")
>splitFilename "/path.to/foo"                                = ("/path.to/foo","")
>splitFilename "a///./x"                                     = ("a/x","")
>splitFilename "dir.suffix/./"                               = ("dir","suffix")
>splitFilename "Photographie, Das 20. Jahrhundert (300 dpi)" = ("Photographie, Das 20", " Jahrhundert (300 dpi)")

See 'slicePath', 'splitFilename\''
-}
splitFilename :: String                -- ^ Path including the file name to be split
               -> (String, String)      -- ^ The normalised path with the file prefix, and the file suffix.
splitFilename "" = ("", "")
splitFilename path =
  case slicePath path of
    []    -> (".","")
    comps -> let (pref_fn, suff_fn) = splitFilename' (last comps)
              in ( intercalate [pathSeparator] (init comps ++ [pref_fn])
                , suff_fn
                )

{- | Variant of 'splitFilename'. This is a more efficient version
of 'splitFilename', for the case that you know the string is
is a pure file name without any slashes.

See 'splitFilename'.
-}
splitFilename' :: String               -- ^ Filename to be split
                -> (String, String)     -- ^ Base name and the last suffix
splitFilename' "" = ("", "")
splitFilename' fn =
  let parts = sliceFilename' fn
  in case parts of
        []     -> (".", "")
        [base] -> (base, "")
        p      -> (unsliceFilename (init p), last p)


{- | Inverse of 'splitFilename'. Concatenate prefix and suffix, adding
a dot in between, iff the suffix is not empty. The path part of the prefix is
normalised.

See 'splitFilename'.
-}
unsplitFilename :: (String, String)    -- ^ File name prefix and suffix
                 -> String              -- ^ Path
unsplitFilename (prefix, suffix) = if suffix == "" then prefix else prefix ++ "." ++ suffix


{- | Split a path in directory, base file name and suffix.
-}
split3 :: String                        -- ^ Path to split
       -> (String, String, String)      -- ^ Directory part, base file name part and suffix part
split3 "" = ("","","")
split3 path =
  let comps = slicePath path
      (base, suffix) = splitFilename' (last comps)
  in  (unslicePath (init comps), base, suffix)


{- |
Form path from directory, base file name and suffix parts.
-}
unsplit3 :: (String, String, String)    -- ^ Directory part, base file name part and suffix part
         -> String                      -- ^ Path consisting of dir, base and suffix
unsplit3 (dir, base, suffix) = unsplitPath (dir, unsplitFilename (base,suffix))


{- | Test a path for a specific suffix and split it off.

If the path ends with the suffix, then the result is @Just
prefix@, where @prefix@ is the normalised path
without the suffix. Otherwise it's @Nothing@.
-}
test_suffix :: String           -- ^ Suffix to split off
            -> String           -- ^ Path to test
            -> Maybe String     -- ^ Prefix without the suffix or @Nothing@
test_suffix suffix path =
  let (prefix, suff) = splitFilename path
  in if suff == suffix then Just prefix else Nothing

{- | Make a path absolute, using the current working directory.

This makes a relative path absolute with respect to the current
working directory. An absolute path is returned unmodified.

The current working directory is determined with @getCurrentDirectory@
which means that symbolic links in it are expanded and the path is
normalised. This is different from @pwd@.
-}
absolutePath :: String         -- ^ The path to be made absolute
              -> IO String      -- ^ Absulte path
absolutePath path = fmap (absolutePath' path) getCurrentDirectory


{- | Make a path absolute.

This makes a relative path absolute with respect to a specified
directory. An absolute path is returned unmodified.
-}
absolutePathBy :: String        -- ^ The directory relative to which the path is made absolute
                 -> String        -- ^ The path to be made absolute
                 -> String        -- ^ Absolute path
absolutePathBy = (</>)


{- | Make a path absolute.

This makes a relative path absolute with respect to a specified
directory. An absolute path is returned unmodified.

The order of the arguments can be confusing. You should rather use 'absolutePathBy'. @absolutePath\'@ is included for backwards compatibility.
-}
absolutePath' :: String        -- ^ The path to be made absolute
               -> String        -- ^ The directory relative to which the path is made absolute
               -> String        -- ^ Absolute path
absolutePath' = flip absolutePathBy


{- | Guess the @\"..\"@-component free form of a path, specified as a list of path components, by syntactically removing them, along with the preceding
   path components. This will produce
   erroneous results when the path contains symlinks. If the path contains leading @\"..\"@ components, or more @\"..\"@ components than preceeding normal
   components, then the @\"..\"@ components can't be normalised away. In this case, the result is @Nothing@.
-}
guessDotdotComps :: [String]          -- ^ List of path components
                   -> Maybe [String]    -- ^ In case the path could be transformed, the @\"..\"@-component free list of path components.
guessDotdotComps = guessDotdotComps' []
   where
      guessDotdotComps' schon [] = Just schon
      guessDotdotComps' [] ("..":_) = Nothing
      guessDotdotComps' schon ("..":teile) = guessDotdotComps' (init schon) teile
      guessDotdotComps' schon (teil:teile) = guessDotdotComps' (schon ++ [teil]) teile


{- | Guess the @\"..\"@-component free, normalised form of a path. The transformation is purely syntactic. @\"..\"@ path components will be removed, along
   with their preceding path components. This will produce
   erroneous results when the path contains symlinks. If the path contains leading @\"..\"@ components, or more @\"..\"@ components than preceeding normal
   components, then the @\"..\"@ components can't be normalised away. In this case, the result is @Nothing@.

>guessDotdot = fmap unslicePath . guessDotdotComps . slicePath
-}
guessDotdot :: String                  -- ^ Path to be normalised
             -> Maybe String            -- ^ In case the path could be transformed, the normalised, @\"..\"@-component free form of the path.
guessDotdot = fmap unslicePath . guessDotdotComps . slicePath

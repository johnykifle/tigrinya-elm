module Asset exposing (Image, defaultAvatar, error, loading, src)

import Html exposing (Attribute, Html)
import Html.Attributes as Attr

-- TYPES
type Image 
    = Image String

-- IMAGES

error: Image
error =
    image "error.jpg"

loading : Image
loading =
    image "loading.svg"

defaultAvatar : Image
defaultAvatar =
    image "smiley-cyrus.jpg"

image : String -> Image
image filename =
    Image ("/assets/images/" ++ filename)

-- USING IMAGES

src : Image -> Attribute msg
src (Image url) =
    Attr.src url


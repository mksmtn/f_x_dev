module CustomAttributes (integrity, crossorigin) where

import Text.Blaze (customAttribute, AttributeValue, Attribute)

integrity :: AttributeValue -> Attribute
integrity = customAttribute "integrity"

crossorigin :: AttributeValue -> Attribute
crossorigin = customAttribute "crossorigin"

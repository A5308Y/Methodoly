module ListHelper exposing (groupsOf)


groupsOf : Int -> List a -> List (List a) -> List (List a)
groupsOf size elementsLeftToGroup groupedElements =
    if List.isEmpty elementsLeftToGroup then
        groupedElements
    else
        groupsOf
            size
            (List.drop size elementsLeftToGroup)
            (List.take size elementsLeftToGroup :: groupedElements)

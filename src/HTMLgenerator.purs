module HTMLgenerator
       ( htmlGenerator1,htmlGenerator2, htmlCombine , htmlShow , HTMLContent )
       where
import RCommand (RCommand, cmdGenerator)


data Tree a = Node {node :: a, ltree :: (Tree a) , rtree :: (Tree a)} | Null
--data List a = Node {node :: a, next :: (List a)} | Null

                   
type PropertyType = String 
type PropertyContent = String
data HTMLProperty = HTMLproperty {ptype :: PropertyType, pcontent :: PropertyContent}
type Properties = Array HTMLProperty

type HTMLElementType = String
                     
data HTMLElement = HTMLelement {eletype :: HTMLElementType , plist :: Properties, text :: String}  

type HTMLContent = Tree HTMLElement


htmlShow :: HTMLContent -> String
htmlShow (Node n left right) = htmlElementPrint n (htmlShow left) (htmlShow right)
htmlShow Null = ""

htmlElementPrint :: HTMLElement -> String ->String ->String
htmlElementPrint (HTMLelement etype properties text) sub peer =
  (htmlETypePrint etype) (htmlPPrint properties) sub peer

htmlPPrint :: Properties -> String
htmlPPrint ((HTMLproperty ptype pcontent):next) = 
  ptype ++ "=" ++ "\""  ++ pcontent ++ "\"" ++ (" " ++ (htmlPPrint next))
htmlPPrint [] = ""
  
htmlETypePrint ::HTMLElementType -> String -> String -> String ->String
htmlETypePrint etype plist sub peer = "<" ++ (etypePrint etype) ++ " " ++ plist ++ ">"
                                      ++ sub
                                      ++ "<" ++ (antietypePrint etype) ++ ">"
                                      ++ peer
  where etypePrint :: HTMLElementType -> String
        etypePrint x = x
        antietypePrint :: HTMLElementType -> String
        antietypePrint x = '/':(etypePrint x)
--The Generator


htmlInit :: String ->(HTMLContent -> HTMLContent) -> HTMLContent
htmlInit titlename f = Node head (Node title Null Null) $ f (Node body Null Null)
  where head = HTMLelement "head" [] ""
        title = HTMLelement "title" [] titlename
        body = HTMLelement "body" [] ""


htmlCombine :: HTMLContent -> HTMLContent -> HTMLContent
htmlCombine
  (Node (HTMLelement "head" [] "") (Node (HTMLelement "title" _ titlename) c1 y1)
   (Node (HTMLelement "body" [] "") body1 c2))
  (Node (HTMLelement "head" [] "") (Node (HTMLelement "title" _ _) _ y2)
   (Node (HTMLelement "body" [] "") body2 _)) =
    (Node (HTMLelement "head" [] "") (Node (HTMLelement "title" [] titlename) c1 (peerCombine y1 y2))
     (Node (HTMLelement "body" [] "") (peerCombine body1 body2) c2))
  where peerCombine (Node n c Null) x = Node n c x
        peerCombine (Node n c l) x = Node n c (peerCombine l x) 

htmlGenerator1 :: String -> Array String -> HTMLContent
htmlGenerator1 x y = htmlInit "" (\(Node x _ _) -> (Node x (htmlGenerator1' x y) Null)) 
  where htmlGenerator1'  _ [] = Null
        htmlGenerator1'  now_path (x:dir) = Node x' checkbox $ htmlGenerator1' dir
          where x' = HTMLelement "p" [] ""
                checkbox = Node checkbox' (Node a) Null
                checkbox' = HTMLelement "input"
                            [HTMLproperty "type" "checkbox",
                             HTMLproperty "name" x,
                             HTMLproperty "value" (now_path ++ x)]
                            ""
                a = HTMLelement "a" [HTMLproperty "href" (cmdGenerator "dir" ((now_path ++ x):[]))] x
                
htmlGenerator2 :: Array RCommand -> HTMLContent
--htmlGenerator2 ahrefs = 

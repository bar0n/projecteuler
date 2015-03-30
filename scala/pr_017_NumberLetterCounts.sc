
def words(int:Int)={
  EnglishNumbers.convert(int)
}
(1 to 1000).map(words).map(x=>x.replace(" ","")).foldLeft("")((x,y)=>x+y).length


EnglishNumbers.convert(1343)
EnglishNumbers.convert(115)
"three hundred and forty two".replace(" ","").length
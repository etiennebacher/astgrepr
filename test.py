from ast_grep_py import SgRoot

src = """
print('hello')
logger('hello', 'world', '!')
"""
root = SgRoot(src, "python").root()
node = root.find(pattern="print($A)")

root.next()

print("Hellooooo world!")

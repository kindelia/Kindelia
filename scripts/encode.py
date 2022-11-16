def char_to_code(c):
    val = ord(c)
    if c == ".":
        return 0
    elif val >= ord("0") and val <= ord("9"):
        return 1 + val - ord("0")
    elif val >= ord("A") and val <= ord("Z"):
        return 11 + val - ord("A")
    elif val >= ord("a") and val <= ord("z"):
        return 37 + val - ord("a")
    else:
        raise ValueError(f"Unsupported character: {c}")

def str_to_code(string):
    num = 0
    for char in string:
        num = (num << 6) + char_to_code(char)
    return num
    
if __name__=="__main__":
    import sys
    
    name = sys.argv[1]
    print(hex(str_to_code(name)))



def romanToInt(s: str) -> int:
    Roman= {"I":1, "V":2, "X":10, "L":50,"C":100, "D":500, "M":1000}
    result = 0
    i = 0
    while i < len(s):
        s1 =  Roman[s[i]]
        if i + 1  < len(s):
            s2  = Roman[s[i+1]]
            if s1 >= s2:
                result = result + s1   
                i+=1
            else:
                result = result - s1
                i+=1
        else:
            result = result + s1
            i+=1
    return result

romanToInt("IV")
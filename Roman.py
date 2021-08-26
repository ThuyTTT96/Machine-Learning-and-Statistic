def romanToInt(s):
    """
    :type s: str
    :rtype: int
    """
    
    res = 0
    RomanList = []
    Roman = {'I':1, 'V': 5, 'X': 10, 'L': 50, 'C':100, 'D':500, 'M':1000,
                'IV': 4, 'IX':9, 'XL': 40, 'XC':90, 'CD':400, 'CM':900}
    SpecRoman = {'IV': 4, 'IX':9, 'XL': 40, 'XC':90, 'CD':400, 'CM':900}
    if s in Roman.keys():
        return Roman[s]
    else:
        characters = list(s)
        print(characters)
        i, j = 0, 1
        while True:
            if i < len(characters) -1:
                if(characters[i]+characters[j]) in SpecRoman :
                    RomanList.append(characters[i]+characters[j])
                    i+=2
                    j+=2
                else:
                    RomanList.append(characters[i])
                    i+=1
                    j+=1
                    
            elif i == len(characters) -1:
                RomanList.append(characters[i])
                break
        print(RomanList)
        for character in RomanList:
            if character in Roman.keys():
                res = res + Roman[character]
        return res


s= 'III'
romanToInt(s)
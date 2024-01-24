prvi, drugi = (
    [
        [60, 50, 40, 30, 20],
        [40, 50, 60, 73, 80],
        [10, 20, 30, 40, 50],
    ],
    [
        [30, 40, 50, 60, 70],
        [40, 60, 30, 20, 40],
        [10, 20, 90, 40, 50],
    ],
)
from functools import cache
def pobeg(breg1, breg2):
    m, n = len(breg1), len(breg1[0])
    @cache
    def energija1(i,j):
        if j == 0 and i == n-1:
            return breg1[j][i]
        elif j> 0 and i < n-1:
            sprememba= breg1[i][j]
            navzgor= energija1(i-1, j) - 12 if i-1 > 0 else 0
            desno= energija1(i, j+1) - 10 if j+1<= m-1 else 0
            desno_navzgor= energija1(i-1, j+1) - 14 if i-1> 0 and j+1 <= m-1 else 0
            return sprememba + max ( navzgor, desno, desno_navzgor)
    @cache
    def energija2(i,j):
            if j == m-1 and i == n-1:
                return breg2[j][i]
            elif j < m and i < n:
                sprememba= breg2[j][i]
                navzdol= energija2(i+1, j) - 12 if i+1 <= n-1 else 0
                desno= energija2(i, j+1) - 10 if j+1 <= m-1 else 0
                desno_navzdol= energija2(i+1, j+1) - 14 if i+1<= n-1 and j+1 <= m-1 else 0
                return sprememba + max ( navzdol, desno, desno_navzdol)

    return energija1(n-1, 0) + energija2(0,0)
module FinalProject.Sample where
import Euterpea

-- Sonata in C Major (Mozart)

sonataInC :: [Music Pitch]
sonataInC = [c 5 wn, e 5 hn, g 5 hn, b 4 dhn, c 5 en, d 5 en, c 5 hn, rest hn, 
             a 5 wn, g 5 hn, c 6 hn, g 5 hn, f 5 en, g 5 en, e 5 en, f 5 en, e 5 hn, rest hn, 
             a 4 qn, b 4 en, c 5 en, d 5 en, e 5 en, f 5 en, g 5 en, a 5 en,
             g 5 en, f 5 en, e 5 en, d 5 en, c 5 en, b 4 en, a 4 en,
             g 4 qn, a 4 en, b 4 en, c 5 en, d 5 en, e 5 en, f 5 en, g 5 en,
             f 5 en, e 5 en, d 5 en, c 5 en, b 4 en, a 4 en, g 4 en,
             f 4 qn, g 4 en, a 4 en, b 4 en, c 5 en, d 5 en, e 5 en, f 5 en,
             e 5 en, d 5 en, c 5 en, b 4 en, a 4 en, g 4 en, f 4 en,
             e 4 qn, f 4 en, g 4 en, a 4 en, b 4 en, c 5 en, d 5 en, e 5 en, 
             d 5 en, c 5 en, b 4 en, a 4 en, g 4 en, f 4 en, e 4 en,
             d 4 qn, e 4 en, f 4 en, g 4 en, a 4 en, b 4 en, cs 5 en,
             d 5 en, a 4 en, b 4 en, cs 5 en, d 5 en, e 5 en, f 5 en, g 5 en,
             a 5 en, b 5 en, c 6 en, b 5 en, a 5 en, g 5 en, f 5 en, e 5 en,
             f 5 en, g 5 en, a 5 en, g 5 en, f 5 en, e 5 en, d 5 en, c 5 en,
             b 4 qn, g 5 qn, e 5 qn, c 5 qn, d 5 qn, g 5 qn, e 5 qn, c 5 qn,
             d 5 hn, g 5 hn, g 4 hn, rest hn]

-- Sonatina (Clementi)

sonatina :: [Music Pitch]
sonatina = [c 5 qn, e 5 en, c 5 en, g 4 qn, g 4 qn, c 5 qn, e 5 en, c 5 en, g 4 qn, g 5 qn,
            f 5 en, e 5 en, d 5 en, c 5 en, b 4 en, c 5 en, b 4 en, c 5 en, d 5 en, c 5 en,
            b 4 en, a 4 en, g 4 qn, rest qn, c 5 qn, e 5 en, c 5 en, g 4 qn, g 4 qn,
            e 5 qn, g 5 en, e 5 en, c 5 qn, e 5 en, c 5 en, d 5 en, b 4 en, c 5 en, a 4 en,
            b 4 en, g 4 en, a 4 en, fs 4 en, g 4 en, a 4 en, b 4 en, c 5 en, d 5 en, e 5 en,
            fs 5 en, g 5 en, a 4 qn, a 5 qn, a 5 qn, a 5 qn, b 4 en, c 5 en, d 5 en, e 5 en,
            fs 5 en, g 5 en, a 5 en, b 5 en, c 5 qn, c 6 qn, c 6 qn, c 6 qn, d 5 en, g 5 en,
            b 5 en, d 6 en, c 6 en, b 5 en, a 5 en, g 5 en, fs 5 en, e 5 en, g 5 en, fs 5 en,
            a 5 en, g 5 en, fs 5 en, e 5 en, e 5 en, d 5 en, c 5 en, b 4 en, d 5 en, c 5 en,
            b 4 en, a 4 en, g 4 hn, rest hn]

-- Sonata No. 7 (Haydn)

sonataNo7 :: [Music Pitch]
sonataNo7 = [g 4 qn, g 4 hn, e 4 qn, c 5 qn, c 5 hn, b 4 qn, d 5 qn, d 5 dqn, e 5 en, f 5 qn,
             f 5 qn, f 5 hn, e 5 qn, g 5 qn, g 5 dhn, gs 5 qn, a 5 dqn, b 5 en, d 6 en, c 6 en,
             b 5 en, a 5 en, g 5 hn, fs 5 en, g 5 en, e 5 en, fs 5 en, g 5 dhn, g 4 qn, g 4 hn,
             e 4 qn, c 5 qn, c 5 hn, b 4 qn, d 5 qn, d 5 dqn, e 5 en, f 5 qn, f 5 qn, f 5 hn,
             e 5 qn, g 5 qn, g 5 dhn, gs 5 qn, a 5 dqn, b 5 en, d 6 en, c 6 en, b 5 en, a 5 en,
             g 5 hn, fs 5 en, g 5 en, e 5 en, fs 5 en, g 5 dhn, g 5 qn]

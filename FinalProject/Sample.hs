module FinalProject.Sample where
import Euterpea

-- Sonata in C Major (Exposition) (Mozart)

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
             d 5 hn, g 5 hn, g 4 hn, rest hn,
             fs 4 en, g 4 en, fs 4 en, g 4 en, fs 4 en, g 4 en, fs 4 en, g 4 en,
             f 4 en, g 4 en, f 4 en, g 4 en, f 4 en, g 4 en, f 4 en, g 4 en,
             g 5 qn, e 5 qn, c 5 dhn, d 5 en, e 5 en, d 5 qn, c 5 qn,
             c 5 dqn, b 4 en, b 4 hn, rest wn, g 5 qn, e 5 qn, c 5 dhn,
             d 5 en, e 5 en, d 5 qn, c 5 qn, c 5 dqn, b 4 en, b 4 hn, rest wn,
             g 5 en, e 3 en,g 3 en, c 4 en, e 4 en, g 5 en, e 5 en, c 5 en,
             a 4 en, f 3 en, a 3 en, c 4 en, f 4 en, a 4 en, c 5 en, a 4 en,
             f 5 en, d 3 en, f 3 en, b 3 en, d 4 en, f 5 en, d 5 en, b 4 en,
             g 4 en, e 3 en, g 3 en, b 3 en, e 4 en, g 4 en, b 4 en, g 4 en,
             e 5 en, c 4 en, e 4 en, a 4 en, c 5 en, e 5 en, c 5 en, a 4 en,
             f 4 en, d 4 en, f 4 en, a 4 en, d 5 en, f 4 en, a 4 en, f 4 en,
             d 6 en, b 3 en, d 4 en, g 4 en, b 4 en, d 6 en, b 5 en, g 5 en,
             e 5 en, c 4 en, e 4 en, g 4 en, c 5 en, c 6 en, g 5 en, e 5 en,
             d 5 wn, d 5 hn, d 5 hn, a 5 wn, a 5 hn, a 5 hn, g 5 qn, a 5 en,
             b 5 en, c 6 en, d 6 en, e 6 en, d 6 en, c 6 en, b 5 en, a 5 en,
             g 5 en, f 5 en, e 5 en, d 5 en, c 5 en, e 5 en, d 5 en, e 5 en,
             d 5 en, e 5 en, d 5 en, e 5 en, d 5 en, e 5 en, d 5 en, e 5 en,
             d 5 en, e 5 en, d 5 en, c 5 en, d 5 en, c 5 hn, c 5 en, g 4 en,
             c 5 en, e 5 en, g 5 en, e 5 en, c 5 en, e 5 en, f 5 en, d 5 en,
             b 4 en, d 5 en, c 5 hn, c 4 en, g 3 en, c 4 en, e 4 en, g 4 en,
             e 4 en, c 4 en, e 4 en, f 4 en, d 4 en, b 3 en, d 4 en, c 4 hn,
             c 5 hn, c 4 hn]

-- Sonatina Op. 36, No. 1 (Clementi)

sonatina :: [Music Pitch]
sonatina = partA ++ partA ++ partB ++ partB where
   partA = [c 5 qn, e 5 en, c 5 en, g 4 qn, g 4 qn, c 5 qn, e 5 en, c 5 en, g 4 qn, g 5 qn,
            f 5 en, e 5 en, d 5 en, c 5 en, b 4 en, c 5 en, b 4 en, c 5 en, d 5 en, c 5 en,
            b 4 en, a 4 en, g 4 qn, rest qn, c 5 qn, e 5 en, c 5 en, g 4 qn, g 4 qn,
            e 5 qn, g 5 en, e 5 en, c 5 qn, e 5 en, c 5 en, d 5 en, b 4 en, c 5 en, a 4 en,
            b 4 en, g 4 en, a 4 en, fs 4 en, g 4 en, a 4 en, b 4 en, c 5 en, d 5 en, e 5 en,
            fs 5 en, g 5 en, a 4 qn, a 5 qn, a 5 qn, a 5 qn, b 4 en, c 5 en, d 5 en, e 5 en,
            fs 5 en, g 5 en, a 5 en, b 5 en, c 5 qn, c 6 qn, c 6 qn, c 6 qn, d 5 en, g 5 en,
            b 5 en, d 6 en, c 6 en, b 5 en, a 5 en, g 5 en, fs 5 en, e 5 en, g 5 en, fs 5 en,
            a 5 en, g 5 en, fs 5 en, e 5 en, e 5 en, d 5 en, c 5 en, b 4 en, d 5 en, c 5 en,
            b 4 en, a 4 en, g 4 hn, rest hn]
   partB = [b 4 qn, d 5 en, b 4 en, g 4 qn, g 4 qn, c 5 qn, ef 5 en, c 5 en, g 4 qn,
            g 5 qn, f 5 qn, d 5 qn, ef 5 qn, c 5 qn, b 4 en, c 5 en, d 5 en, b 4 en,
            g 4 qn, g 4 qn, g 5 en, g 4 en,  g 5 en, g 4 en, g 5 en, g 4 en, g 5 en,
            g 4 en, g 5 en, g 4 en, g 5 en, g 4 en, g 5 en, g 4 en, g 5 en, g 4 en,
            d 5 en, ef 5 en, f 5 en, d 5 en, f 5 en, ef 5 en, d 5 en, c 5 en, g 5 qn,
            rest qn, rest hn, c 4 qn, e 4 en, c 4 en, g 3 qn, g 3 qn, c 4 qn, e 4 en,
            c 4 en, g 3 qn, g 4 qn, f 4 en, e 4 en, d 4 en, c 4 en, b 3 en, c 4 en,
            b 3 en, c 4 en, d 4 en, c 4 en, b 3 en, a 3 en, g 3 qn, rest qn, c 4 qn,
            g 3 en, c 3 en, e 4 qn, e 4 qn, e 4 qn, c 4 en, e 4 en, g 4 qn, c 5 qn,
            g 4 qn, f 4 qn, e 4 qn, d 4 qn, c 4 en, d 4 en, e 4 en, f 4 en, g 4 en,
            a 4 en, b 4 en, c 5 en, d 4 qn, d 5 qn, d 5 qn, d 5 qn, e 4 en, f 4 en,
            g 4 en, a 4 en, b 4 en, c 5 en, d 5 en, e 5 en, f 4 qn, f 5 qn, f 5 qn,
            f 5 qn, g 4 en, c 5 en, e 5 en, g 5 en, f 5 en, e 5 en, d 5 en, c 5 en,
            a 5 en, g 5 en, f 5 en, e 5 en, d 5 en, c 5 en, b 4 en, a 4 en, g 4 en,
            a 4 en, f 4 en, g 4 en, e 4 en, f 4 en, d 4 en, e 4 en, c 4 qn, rest dhn]

-- Sonata No. 7 (Haydn)

sonataNo7 :: [Music Pitch]
sonataNo7 = [g 4 qn, g 4 hn, e 4 qn, c 5 qn, c 5 hn, b 4 qn, d 5 qn, d 5 dqn, e 5 en, f 5 qn,
             f 5 qn, f 5 hn, e 5 qn, g 5 qn, g 5 dhn, gs 5 qn, a 5 dqn, b 5 en, d 6 en, c 6 en,
             b 5 en, a 5 en, g 5 hn, fs 5 en, g 5 en, e 5 en, fs 5 en, g 5 dhn, g 4 qn, g 4 hn,
             e 4 qn, c 5 qn, c 5 hn, b 4 qn, d 5 qn, d 5 dqn, e 5 en, f 5 qn, f 5 qn, f 5 hn,
             e 5 qn, g 5 qn, g 5 dhn, gs 5 qn, a 5 dqn, b 5 en, d 6 en, c 6 en, b 5 en, a 5 en,
             g 5 hn, fs 5 en, g 5 en, e 5 en, fs 5 en, g 5 dhn, g 5 qn]

# ramb(iguous)

<img width="420" alt="image" src="https://github.com/user-attachments/assets/0ef01453-cc72-4a4b-b1d1-5ac318d0b342" />
<br/>
<br/>

`ramb` provides an implementation of the `amb` operator for Racket. This implementation is largely based on the one descriped in [this section](https://docs.scheme.org/tyscheme/index-Z-H-16.html#TAG:__tex2page_sec_14.2) of "Teach Yourself Scheme in Fixnum Days" by Dorai Sitaram.

## Usage

Here is a simple program that uses `ramb` to find pairs `(x, y)` such that `x` is in the list `(1 2 3 4 5)` and `y` is in the list `(6 7 8 9 10`) such that their sum is 8.

```racket
(define solve/sum-to-8
  (λ ()
    (let ([a (amb 1 2 3 4 5)]
          [b (amb 6 7 8 9 10)])
      (assert (equal? (+ a b) 8))
      (list a b))))
(bag-of (solve/sum-to-8))
```

A few things to note here:
- `bag-of` is a method that enables you to return all solutions (`ramb` by default will return the first possible solution)
- `ramb` performs chronological backtracking on your search space

A more sophisticated example is the following program that solves the map coloring problem described [here](https://www.metalevel.at/prolog/optimization). Here is the map from the problem:

<img width="385" alt="image" src="https://github.com/user-attachments/assets/4843fe57-a5a0-468e-a20f-34bcbc3cf819" />



```racket
(define map-colors (list 'red 'green 'blue 'yellow))
(define adjacency-list
  (hash 'a '(b c d f)
        'b '(a c d)
        'c '(a b d e)
        'd '(a b c e f)
        'e '(c d f)
        'f '(a d e)))

(define solve/map-coloring
  (λ ()
    (let ([node-colors
           (apply hash
                  (append-map (λ (node)
                                (list node (amb/list map-colors)))
                              '(a b c d e f)))])
      (assert (andmap
               (λ (kv)
                 (let* ([node (car kv)]
                        [node-color (cdr kv)]
                        [neighbour-colors
                         (map
                          (λ (neighbour)
                            (hash-ref node-colors neighbour))
                          (hash-ref adjacency-list node))])
                   (not (member node-color neighbour-colors)))) 
               (hash->list node-colors)))
      (displayln node-colors))))
(solve/map-coloring)
```

Finally, here is an example program that solves the [8 queens problem](https://en.wikipedia.org/wiki/Eight_queens_puzzle):
```racket
(define solve/8-queens
  (λ ()
    ;; only assign columns since the row assignments are implicit
    ;; (i.e must be 1, 2, 3,..., 8)
    (define queens (build-list 8 (λ (_) (number-between 1 8))))
    (assert (and
             ;; check for same rows
             (equal? (length queens) (length (remove-duplicates queens)))
             ;; check for same diagonals
             (andmap (λ (i)
                       (andmap (λ (j)
                                 (not
                                  (=
                                   (abs
                                    (- (list-ref queens i)
                                       (list-ref queens j)))
                                   (abs (- i j)))))
                               (range i)))
                     (range (length queens)))))
    (displayln queens)))
(solve/8-queens)
```

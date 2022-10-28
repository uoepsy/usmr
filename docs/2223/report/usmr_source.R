get_my_data <- function(exam_num = NULL){
  if(is.null(exam_num) | is.na(as.numeric(gsub("[^\\d]+", "", exam_num, perl=TRUE)))){
    print("PLEASE SUPPLY YOUR EXAM NUMBER")
  } else {
    set.seed(exam_num)
    require(tidyverse)
    
    names = c("Louis Agassiz", "Maria Gaetana Agnesi", "Al-Battani", "Abu Nasr Al-Farabi", "Alhazen", "Jim Al-Khalili", "Muhammad ibn Musa al-Khwarizmi", "Mihailo Petrovic Alas", "Angel Alcala", "Salim Ali", "Luis Alvarez", "Andre Marie Ampere", "Anaximander", "Carl Anderson", "Mary Anning", "Virginia Apgar", "Archimedes", "Agnes Arber", "Aristarchus", "Aristotle", "Svante Arrhenius", "Oswald Avery", "Amedeo Avogadro", "Avicenna", "Charles Babbage", "Francis Bacon", "Alexander Bain", "John Logie Baird", "Joseph Banks", "Ramon Barba", "John Bardeen", "Charles Barkla", "Ibn Battuta", "William Bayliss", "George Beadle", "Arnold Orville Beckman", "Henri Becquerel", "Emil Adolf Behring", "Alexander Graham Bell", "Emile Berliner", "Claude Bernard", "Timothy John Berners-Lee", "Daniel Bernoulli", "Jacob Berzelius", "Henry Bessemer", "Hans Bethe", "Homi Jehangir Bhabha", "Alfred Binet", "Clarence Birdseye", "Kristian Birkeland", "James Black", "Elizabeth Blackwell", "Alfred Blalock", "Katharine Burr Blodgett", "Franz Boas", "David Bohm", "Aage Bohr", "Niels Bohr", "Ludwig Boltzmann", "Max Born", "Carl Bosch", "Robert Bosch", "Jagadish Chandra Bose", "Satyendra Nath Bose", "Walther Wilhelm Georg Bothe", "Robert Boyle", "Lawrence Bragg", "Tycho Brahe", "Brahmagupta", "Hennig Brand", "Georg Brandt", "Wernher Von Braun", "J Harlen Bretz", "Louis de Broglie", "Alexander Brongniart", "Robert Brown", "Michael E. Brown", "Lester R. Brown", "Eduard Buchner", "Linda Buck", "William Buckland", "Georges-Louis Leclerc", "Comte de Buffon", "Robert Bunsen", "Luther Burbank", "Jocelyn Bell Burnell", "Macfarlane Burnet", "Thomas Burnet", "Benjamin Cabrera", "Santiago Ramon y Cajal", "Rachel Carson", "George Washington Carver", "Henry Cavendish", "Anders Celsius", "James Chadwick", "Subrahmanyan Chandrasekhar", "Erwin Chargaff", "Noam Chomsky", "Steven Chu", "Leland Clark", "John Cockcroft", "Arthur Compton", "Nicolaus Copernicus", "Gerty Theresa Cori", "Charles-Augustin de Coulomb", "Jacques Cousteau", "Brian Cox", "Francis Crick", "James Croll", "Nicholas Culpeper", "Marie Curie", "Pierre Curie", "Georges Cuvier", "Adalbert Czerny", "Gottlieb Daimler", "John Dalton", "James Dwight Dana", "Charles Darwin", "Humphry Davy", "Peter Debye", "Max Delbruck", "Jean Andre Deluc", "Democritus", "Rene Descartes", "Rudolf Christian Karl Diesel", "Diophantus", "Paul Dirac", "Prokop Divis", "Theodosius Dobzhansky", "Frank Drake", "K. Eric Drexler", "John Eccles", "Arthur Eddington", "Thomas Edison", "Paul Ehrlich", "Albert Einstein", "Gertrude Elion", "Empedocles", "Eratosthenes", "Euclid", "Eudoxus of Cnidus", "Leonhard Euler", "Michael Faraday", "Pierre de Fermat", "Enrico Fermi", "Richard Feynman", "Fibonacci Leonardo of Pisa", "Emil Fischer", "Ronald Fisher", "Alexander Fleming", "Ambrose Fleming", "Howard Florey", "Henry Ford", "Lee De Forest", "Dian Fossey", "Leon Foucault", "Benjamin Franklin", "Rosalind Franklin", "Sigmund Freud", "Elizebeth Smith Friedman", "Galen", "Galileo Galilei", "Francis Galton", "Luigi Galvani", "George Gamow", "Martin Gardner", "Carl Friedrich Gauss", "Murray Gell-Mann", "Sophie Germain", "Willard Gibbs", "William Gilbert", "Sheldon Lee Glashow", "Robert Goddard", "Maria Goeppert-Mayer", "Thomas Gold", "Jane Goodall", "Stephen Jay Gould", "Otto von Guericke", "Fritz Haber", "Ernst Haeckel", "Otto Hahn", "Albrecht von Haller", "Edmund Halley", "Alister Hardy", "Thomas Harriot", "William Harvey", "Stephen Hawking", "Otto Haxel", "Werner Heisenberg", "Hermann von Helmholtz", "Jan Baptist von Helmont", "Joseph Henry", "Caroline Herschel", "John Herschel", "William Herschel", "Gustav Ludwig Hertz", "Heinrich Hertz", "Karl F. Herzfeld", "George de Hevesy", "Antony Hewish", "David Hilbert", "Maurice Hilleman", "Hipparchus", "Hippocrates", "Shintaro Hirase", "Dorothy Hodgkin", "Robert Hooke", "Frederick Gowland Hopkins", "William Hopkins", "Grace Murray Hopper", "Frank Hornby", "Jack Horner", "Bernardo Houssay", "Fred Hoyle", "Edwin Hubble", "Alexander von Humboldt", "Zora Neale Hurston", "James Hutton", "Christiaan Huygens", "Hypatia", "Ernesto Illy", "Ernst Ising", "Keisuke Ito", "Mae Carol Jemison", "Edward Jenner", "J. Hans D. Jensen", "Irene Joliot-Curie", "James Prescott Joule", "Percy Lavon Julian", "Michio Kaku", "Heike Kamerlingh Onnes", "Pyotr Kapitsa", "Friedrich August Kekule", "Frances Kelsey", "Pearl Kendrick", "Johannes Kepler", "Abdul Qadeer Khan", "Omar Khayyam", "Alfred Kinsey", "Gustav Kirchoff", "Martin Klaproth", "Robert Koch", "Emil Kraepelin", "Thomas Kuhn", "Stephanie Kwolek", "Joseph-Louis Lagrange", "Jean-Baptiste Lamarck", "Hedy Lamarr", "Edwin Herbert Land", "Karl Landsteiner", "Pierre-Simon Laplace", "Max von Laue", "Antoine Lavoisier", "Ernest Lawrence", "Henrietta Leavitt", "Antonie van Leeuwenhoek", "Inge Lehmann", "Gottfried Leibniz", "Georges Lemaeetre", "Leonardo da Vinci", "Niccolo Leoniceno", "Aldo Leopold", "Rita Levi-Montalcini", "Claude Levi-Strauss", "Willard Frank Libby", "Justus von Liebig", "Carolus Linnaeus", "Joseph Lister", "John Locke", "Hendrik Antoon Lorentz", "Konrad Lorenz", "Ada Lovelace", "Percival Lowell", "Lucretius", "Charles Lyell", "Trofim Lysenko", "Ernst Mach", "Marcello Malpighi", "Jane Marcet", "Guglielmo Marconi", "Lynn Margulis", "Barry Marshall", "Polly Matzinger", "Matthew Maury", "James Clerk Maxwell", "Ernst Mayr", "Barbara McClintock", "Lise Meitner", "Gregor Mendel", "Dmitri Mendeleev", "Franz Mesmer", "Antonio Meucci", "John Michell", "Albert Abraham Michelson", "Thomas Midgeley Jr.", "Milutin Milankovi?", "Maria Mitchell", "Mario Molina", "Thomas Hunt Morgan", "Samuel Morse", "Henry Moseley", "Ukichiro Nakaya", "John Napier", "Giulio Natta", "John Needham", "John von Neumann", "Thomas Newcomen", "Isaac Newton", "Charles Nicolle", "Florence Nightingale", "Tim Noakes", "Alfred Nobel", "Emmy Noether", "Christiane Nusslein-Volhard", "Bill Nye", "Hans Christian Oersted", "Georg Ohm", "J. Robert Oppenheimer", "Wilhelm Ostwald", "William Oughtred", "Blaise Pascal", "Louis Pasteur", "Wolfgang Ernst Pauli", "Linus Pauling", "Randy Pausch", "Ivan Pavlov", "Cecilia Payne-Gaposchkin", "Wilder Penfield", "Marguerite Perey", "William Perkin", "John Philoponus", "Jean Piaget", "Philippe Pinel", "Max Planck", "Pliny the Elder", "Henri Poincare", "Karl Popper", "Beatrix Potter", "Joseph Priestley", "Proclus", "Claudius Ptolemy", "Pythagoras", "Adolphe Quetelet", "Harriet Quimby", "Thabit ibn Qurra", "C. V. Raman", "Srinivasa Ramanujan", "William Ramsay", "John Ray", "Prafulla Chandra Ray", "Francesco Redi", "Sally Ride", "Bernhard Riemann", "Wilhelm Rontgen", "Hermann Rorschach", "Ronald Ross", "Ibn Rushd", "Ernest Rutherford", "Carl Sagan", "Abdus Salam", "Jonas Salk", "Frederick Sanger", "Alberto Santos-Dumont", "Walter Schottky", "Erwin Schrodinger", "Theodor Schwann", "Glenn Seaborg", "Hans Selye", "Charles Sherrington", "Gene Shoemaker", "Ernst Werner von Siemens", "George Gaylord Simpson", "B. F. Skinner", "William Smith", "Frederick Soddy", "Mary Somerville", "Arnold Sommerfeld", "Hermann Staudinger", "Nicolas Steno", "Nettie Stevens", "William John Swainson", "Leo Szilard", "Niccolo Tartaglia", "Edward Teller", "Nikola Tesla", "Thales of Miletus", "Theon of Alexandria", "Benjamin Thompson", "J. J. Thomson", "William Thomson", "Henry David Thoreau", "Kip S. Thorne", "Clyde Tombaugh", "Susumu Tonegawa", "Evangelista Torricelli", "Charles Townes", "Youyou Tu", "Alan Turing", "Neil deGrasse Tyson", "Harold Urey", "Craig Venter", "Vladimir Vernadsky", "Andreas Vesalius", "Rudolf Virchow", "Artturi Virtanen", "Alessandro Volta", "Selman Waksman", "George Wald", "Alfred Russel Wallace", "John Wallis", "Ernest Walton", "James Watson", "James Watt", "Alfred Wegener", "John Archibald Wheeler", "Maurice Wilkins", "Thomas Willis", "E. O. Wilson", "Sven Wingqvist", "Sergei Winogradsky", "Carl Woese", "Friedrich Wohler", "Wilbur and Orville Wright", "Wilhelm Wundt", "Chen-Ning Yang", "Ahmed Zewail")
    
    N = 150
    pairdf <- tibble(
      rearing = sample(c("wild","captv"),N,TRUE),
      
      HOIc = rnorm(N,.7,1.6),
      HOIw = rnorm(N,0,1),
      HOI = round(rnorm(1,0,.1) + scale(ifelse(rearing=="captv",HOIc, HOIw))[,1],3),
      
      species = rep(c("macaque","bonobo","capuchin"), e=N/3),
      friendliness = round(rnorm(N),3),
      age = round(rnorm(1,16,1) + scale(1*friendliness + rnorm(N,0,2))[,1]*5),
      openness = round(rnorm(N),3),
      neuroticism = round(rnorm(N),3),
      dominance = round(rnorm(N),3),
      food_type = rep(rep(c("n","g"),e=N/6), 3)
    )
    
    coefs = c(0, 2.4, 0.03, -3.4, -1.9, -4, -0.03, 0.05, -4.7)
    y = model.matrix(lm(rnorm(N) ~ rearing + age + friendliness + HOI + species + species:HOI, pairdf)) %*% coefs + 
      rnorm(N,0,6)
    y = rnorm(1, 12, 1) + (scale(y)[,1]*rnorm(1,4,.1))
    y = round(y, 1)
    pairdf$task_time = pmax(1,y)
    
    coefs2 = c(0, 9, 3, 2, 5, 4, -14, 6, 25, 35, 0)
    pp = model.matrix(lm(rnorm(N) ~ rearing + age + HOI + dominance + openness + neuroticism + friendliness + food_type + species, 
                         pairdf %>% mutate_if(is.numeric,~scale(.)[,1]))) %*% coefs2
    pairdf$share = rbinom(N, 1, prob = plogis(scale(pp)))
    
    
    # make the data realistic
    pairdf$pid = sample(names, nrow(pairdf))
    pairdf %>% mutate(
      age = pmin(45, pmax(0, age)),
      task_time = pmin(60, pmax(1, task_time))
    )
    
    
    
    
    
    
    taskdata <<- pairdf %>% select(pid, HOI, food_type, task_time, share)
    monkeydata <<- pairdf %>% select(pid, species, rearing, age, dominance, openness, neuroticism, friendliness) %>% sample_n(n())
  }
}

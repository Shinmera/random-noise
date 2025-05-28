(in-package #:org.shirakumo.random-noise)

;; hash.lisp
(docs:define-docs
  (type xxhash
    "Representation of a hashing state.

This is an (UNSIGNED-BYTE 32) and must be used for every function that
accepts an XXHASH argument. If you would like to initialise the hash
based on a seed, it is preferable to use the XXHASH function to
initially hash the seed, rather than using the seed directly.")
  
  (function xxhash
    "Construct an xxhash from a seed.

The seed should be an (UNSIGNED-BYTE 32). If none is provided, 1 is
used instead.

See XXHASH (type)"))

;; sample.lisp
(docs:define-docs
  (type point
    "Representation of a point in a dimensional space.

For 1D this is a single-float. For any higher dimension this is a
(SIMPLE-ARRAY SINGLE-FLOAT (SIZE)). If no size is given, this type is
a combination of all possible types it may return.")

  (type sample
    "Representation of a noise sample as multiple values.

The values of a sample are four single floats, making up

  The actual noise value
  The partial derivative in x
  The partial derivative in y
  The partial derivative in z

If the partial derivative does not exist (such as in z for a 1d or 2d
noise generator) then that derivative is constantly 0.

See SAMPLE
See WITH-SAMPLE")
  
  (function sample
    "Return a sample

This normalises the return value to always be a SAMPLE type,
regardless of the available derivatives.

See SAMPLE (type)")
  
  (function with-sample
    "Bind the sample components to local variables.

SPEC may either be a list of bindings in order of VALUE, DX, DY, DZ,
or it may be a single symbol, which is used for VALUE, wherein the
derivative symbols are derived from that symbol by appending DX, DY,
DZ respectively.

See SAMPLE (type)
See WITH-SAMPLES")
  
  (function with-samples
    "Bind one or more samples to local variables.

The samples are bound as if by LET*, where each element in SPECS is
made up of the binding spec and the sample value form, as per
WITH-SAMPLE.

See SAMPLE (type)
See WITH-SAMPLE")
  
  (function smoothstep
    "Return a smoothed version of the passed sample parts.

See SMOOTHSTEP!
See SAMPLE (type)")
  
  (function smoothstep!
    "Return a smoothed version of the given sample value.

See SMOOTHSTEP
See SAMPLE (type)")
  
  (function turbulence
    "Return the sample parts after applying the turbulence operator to them.

See TURBULENCE!
See SAMPLE (type)")
  
  (function turbulence!
    "Return the given sample value after applying the turbulence operator to it.

See TURBULENCE
See SAMPLE (type)")
  
  (function sample/1d
    "Sample a noise function in the domain [0,1] with the given resolution.

Returns two arrays: the values, and the partial derivatives. All
arrays have length (resolution+1)^1.

The FUNCTION must be a sampling function that returns a SAMPLE and
accepts (at least) the following arguments in order:
  POINT FREQUENCY XXHASH
To each call of the sampling function the ARGS are appended as well.

See XXHASH (type)
See POINT (type)
See SAMPLE (type)")
  
  (function sample/2d
    "Sample a noise function in the domain [[0,1],[0,1]] with the given resolution.

Returns three arrays: the values, and the partial derivatives. All
arrays have length (resolution+1)^2.

The FUNCTION must be a sampling function that returns a SAMPLE and
accepts (at least) the following arguments in order:
  POINT FREQUENCY XXHASH
To each call of the sampling function the ARGS are appended as well.

See XXHASH (type)
See POINT (type)
See SAMPLE (type)")
  
  (function sample/3d
    "Sample a noise function in the domain [[0,1],[0,1],[0,1]] with the given resolution.

Returns four arrays: the values, and the partial derivatives. All
arrays have length (resolution+1)^3.

The FUNCTION must be a sampling function that returns a SAMPLE and
accepts (at least) the following arguments in order:
  POINT FREQUENCY XXHASH
To each call of the sampling function the ARGS are appended as well.

See XXHASH (type)
See POINT (type)
See SAMPLE (type)"))

;; lattice.lisp
(docs:define-docs
  (type lattice
    "Internal representation of lattice data.

See LATTICE")

  (function normal-lattice
    "Generates a standard, non-repeating lattice.

See TILING-LATTICE
see LATTICE")
  
  (function tiling-lattice
    "Generates a repeating lattice that tiles smoothly across its boundaries.

See NORMAL-LATTICE
see LATTICE")
  
  (function lattice/1d
    "Generate a 1D noise sample based on a lattice and gradient function.

The lattice should be a function of three arguments:
  COORDINATE, FREQUENCY, LATTICE
Wherein it writes the lattice information into the provided final
argument.

The gradient should be a function of two arguments:
  XXHASH, X-COORDINATE

See LATTICE
See NORMAL-LATTICE
See TILING-LATTICE
See XXHASH (type)
See POINT (type)
See SAMPLE (type)")
  
  (function lattice/2d
    "Generate a 2D noise sample based on a lattice and gradient function.

The lattice should be a function of three arguments:
  COORDINATE, FREQUENCY, LATTICE
Wherein it writes the lattice information into the provided final
argument.

The gradient should be a function of three arguments:
  XXHASH, X-COORDINATE, Y-COORDINATE

See LATTICE
See NORMAL-LATTICE
See TILING-LATTICE
See XXHASH (type)
See POINT (type)
See SAMPLE (type)")
  
  (function lattice/3d
    "Generate a 3D noise sample based on a lattice and gradient function.

The lattice should be a function of three arguments:
  COORDINATE, FREQUENCY, LATTICE
Wherein it writes the lattice information into the provided final
argument.

The gradient should be a function of four arguments:
  XXHASH, X-COORDINATE, Y-COORDINATE, Z-COORDINATE

See LATTICE
See NORMAL-LATTICE
See TILING-LATTICE
See XXHASH (type)
See POINT (type)
See SAMPLE (type)")
  
  (function lattice
    "Generate a noise sample based on a lattice and gradient function.

The lattice should be a function of three arguments:
  COORDINATE, FREQUENCY, LATTICE
Wherein it writes the lattice information into the provided final
argument.

The gradient should be a function of four arguments:
  XXHASH, X-COORDINATE, Y-COORDINATE, Z-COORDINATE

See LATTICE/1D
See LATTICE/2D
See LATTICE/3D
See NORMAL-LATTICE
See TILING-LATTICE
See XXHASH (type)
See POINT (type)
See SAMPLE (type)"))

;; value.lisp
(docs:define-docs
  (function value-gradient/1d
    "Draw a sample from the value gradient at the given position.

See VALUE/1D
See LATTICE")
  
  (function value-gradient/2d
    "Draw a sample from the value gradient at the given position.

See VALUE/2D
See LATTICE")
  
  (function value-gradient/3d
    "Draw a sample from the value gradient at the given position.

See VALUE/3D
See LATTICE")
  
  (function value/1d
    "Generate a 1D value noise sample.

This is a shorthand for generating lattice noise based on the value
gradient.

See LATTICE
See VALUE
See VALUE-GRADIENT/1D
See XXHASH (type)
See POINT (type)
See SAMPLE (type)")
  
  (function value/2d
    "Generate a 2D value noise sample.

This is a shorthand for generating lattice noise based on the value
gradient.

See LATTICE
See VALUE
See VALUE-GRADIENT/2D
See XXHASH (type)
See POINT (type)
See SAMPLE (type)")
  
  (function value/3d
    "Generate a 3D value noise sample.

This is a shorthand for generating lattice noise based on the value
gradient.

See LATTICE
See VALUE
See VALUE-GRADIENT/3D
See XXHASH (type)
See POINT (type)
See SAMPLE (type)")
  
  (function value
    "Generate a value noise sample.

This is a shorthand for generating lattice noise based on the value
gradient.

See LATTICE
See VALUE/1D
See VALUE/2D
See VALUE/3D
See XXHASH (type)
See POINT (type)
See SAMPLE (type)"))

;; perlin.lisp
(docs:define-docs
  (function perlin-gradient/1d
    "Draw a sample from the perlin gradient at the given position.

See PERLIN/1D
See LATTICE")
  
  (function perlin-gradient/2d
    "Draw a sample from the perlin gradient at the given position.

See PERLIN/2D
See LATTICE")
  
  (function perlin-gradient/3d
    "Draw a sample from the perlin gradient at the given position.

See PERLIN/3D
See LATTICE")
  
  (function perlin/1d
    "Generate a 1D perlin noise sample.

This is a shorthand for generating lattice noise based on the perlin
gradient.

See LATTICE
See PERLIN
See PERLIN-GRADIENT/1D
See XXHASH (type)
See POINT (type)
See SAMPLE (type)")
  
  (function perlin/2d
    "Generate a 2D perlin noise sample.

This is a shorthand for generating lattice noise based on the perlin
gradient.

See LATTICE
See PERLIN
See PERLIN-GRADIENT/2D
See XXHASH (type)
See POINT (type)
See SAMPLE (type)")
  
  (function perlin/3d
    "Generate a 3D perlin noise sample.

This is a shorthand for generating lattice noise based on the perlin
gradient.

See LATTICE
See PERLIN
See PERLIN-GRADIENT/3D
See XXHASH (type)
See POINT (type)
See SAMPLE (type)")
  
  (function perlin
    "Generate a perlin noise sample.

This is a shorthand for generating lattice noise based on the perlin
gradient.

See LATTICE
See VALUE/1D
See VALUE/2D
See VALUE/3D
See XXHASH (type)
See POINT (type)
See SAMPLE (type)"))

;; simplex.lisp
(docs:define-docs
  (function simplex-gradient/1d
    "Draw a sample from the simplex gradient at the given position.

See SIMPLEX/1D
See LATTICE")
  
  (function simplex-gradient/2d
    "Draw a sample from the simplex gradient at the given position.

See SIMPLEX/2D
See LATTICE")
  
  (function simplex-gradient/3d
    "Draw a sample from the simplex gradient at the given position.

See SIMPLEX/3D
See LATTICE")
  
  (function simplex/1d
    "Generate a 1D simplex noise sample.

This is a shorthand for generating lattice noise based on the simplex
gradient.

See LATTICE
See SIMPLEX
See SIMPLEX-GRADIENT/1D
See XXHASH (type)
See POINT (type)
See SAMPLE (type)")
  
  (function simplex/2d
    "Generate a 2D simplex noise sample.

This is a shorthand for generating lattice noise based on the simplex
gradient.

See LATTICE
See SIMPLEX
See SIMPLEX-GRADIENT/2D
See XXHASH (type)
See POINT (type)
See SAMPLE (type)")
  
  (function simplex/3d
    "Generate a 3D simplex noise sample.

This is a shorthand for generating lattice noise based on the simplex
gradient.

See LATTICE
See SIMPLEX
See SIMPLEX-GRADIENT/3D
See XXHASH (type)
See POINT (type)
See SAMPLE (type)")
  
  (function simplex
    "Generate a simplex noise sample.

This is a shorthand for generating lattice noise based on the simplex
gradient.

See LATTICE
See SIMPLEX/1D
See SIMPLEX/2D
See SIMPLEX/3D
See XXHASH (type)
See POINT (type)
See SAMPLE (type)"))

;; fractal.lisp
(docs:define-docs
  (function fractal/1d
    "Generate a 1D fractal noise sample.

Fractal noise is generated based on another noise generator and
combining the noise at different resolutions.

The GENERATOR must be a sampling function that returns a SAMPLE and
accepts the following arguments in order:
  POINT FREQUENCY XXHASH

OCTAVES is the number of resolutions to sample from. At each
successive resolution, the frequency is attenuated by the LACUNARITY
factor, and the amplitude of the signal is attenuated by the
PERSISTENCE factor.

See FRACTAL
See XXHASH (type)
See POINT (type)
See SAMPLE (type)")
  
  (function fractal/2d
    "Generate a 2D fractal noise sample.

Fractal noise is generated based on another noise generator and
combining the noise at different resolutions.

The GENERATOR must be a sampling function that returns a SAMPLE and
accepts the following arguments in order:
  POINT FREQUENCY XXHASH

OCTAVES is the number of resolutions to sample from. At each
successive resolution, the frequency is attenuated by the LACUNARITY
factor, and the amplitude of the signal is attenuated by the
PERSISTENCE factor.

See FRACTAL
See XXHASH (type)
See POINT (type)
See SAMPLE (type)")
  
  (function fractal/3d
    "Generate a 3D fractal noise sample.

Fractal noise is generated based on another noise generator and
combining the noise at different resolutions.

The GENERATOR must be a sampling function that returns a SAMPLE and
accepts the following arguments in order:
  POINT FREQUENCY XXHASH

OCTAVES is the number of resolutions to sample from. At each
successive resolution, the frequency is attenuated by the LACUNARITY
factor, and the amplitude of the signal is attenuated by the
PERSISTENCE factor.

See FRACTAL
See XXHASH (type)
See POINT (type)
See SAMPLE (type)")
  
  (function fractal
    "Generate a fractal noise sample.

Fractal noise is generated based on another noise generator and
combining the noise at different resolutions.

The GENERATOR must be a sampling function that returns a SAMPLE and
accepts the following arguments in order:
  POINT FREQUENCY XXHASH

OCTAVES is the number of resolutions to sample from. At each
successive resolution, the frequency is attenuated by the LACUNARITY
factor, and the amplitude of the signal is attenuated by the
PERSISTENCE factor.

See FRACTAL/1D
See FRACTAL/2D
See FRACTAL/3D
See XXHASH (type)
See POINT (type)
See SAMPLE (type)"))

;; voronoi.lisp
(docs:define-docs
  (type voronoi-method
    "Description of a voronoi distance method.

This object is opaque.

See VORONOI-METHOD
See VORONOI")

  (function voronoi-method
    "Returns a voronoi-method instance for the requested type and arity.

If no such method exists, an error is returned.

The following methods are provided by the library:

  :WORLEY
  :SMOOTH-WORLEY
  :CHEBYSHEV

See VORONOI-METHOD (type)
See VORONOI")
  
  (function voronoi/1d
    "Generate a 1D voronoi noise sample.

See VORONOI
See NORMAL-LATTICE
See TILING-LATTICE
See XXHASH (type)
See POINT (type)
See SAMPLE (type)")
  
  (function voronoi/2d
    "Generate a 2D voronoi noise sample.

See VORONOI
See NORMAL-LATTICE
See TILING-LATTICE
See XXHASH (type)
See POINT (type)
See SAMPLE (type)")
  
  (function voronoi/3d
    "Generate a 3D voronoi noise sample.

See VORONOI
See NORMAL-LATTICE
See TILING-LATTICE
See XXHASH (type)
See POINT (type)
See SAMPLE (type)")
  
  (function voronoi
    "Generate a voronoi noise sample.

Voronoi noise is generated based on a lattice, a voronoi distance
method, and a combination function.

The lattice should be a function of three arguments:
  COORDINATE, FREQUENCY, LATTICE
Wherein it writes the lattice information into the provided final
argument.

METHOD must be a VORONOI-METHOD instance appropriate for this
function's dimensionality.

FUNCTION may either be :F1, :F2, or :F2-F1, selecting the first, the
second, or the subtraction of the samples respectively.

See NORMAL-LATTICE
See TILING-LATTICE
See VORONOI/1D
See VORONOI/2D
See VORONOI/3D
See VORONOI-METHOD
See XXHASH (type)
See POINT (type)
See SAMPLE (type)"))

;; worley.lisp
(docs:define-docs
  (function worley/1d
    "Generate a 1D voronoi-worley noise sample.

This is a shorthand for using VORONOI/1D with the :WORLEY
VORONOI-METHOD.

See WORLEY
See VORONOI
See VORONOI-METHOD
See XXHASH (type)
See POINT (type)
See SAMPLE (type)")
  
  (function worley/2d
    "Generate a 2D voronoi-worley noise sample.

This is a shorthand for using VORONOI/2D with the :WORLEY
VORONOI-METHOD.

See WORLEY
See VORONOI
See VORONOI-METHOD
See XXHASH (type)
See POINT (type)
See SAMPLE (type)")
  
  (function worley/3d
    "Generate a 3D voronoi-worley noise sample.

This is a shorthand for using VORONOI/3D with the :WORLEY
VORONOI-METHOD.

See WORLEY
See VORONOI
See VORONOI-METHOD
See XXHASH (type)
See POINT (type)
See SAMPLE (type)")
  
  (function worley
    "Generate a voronoi-worley noise sample.

This is a shorthand for using VORONOI with the :WORLEY
VORONOI-METHOD.

See WORLEY/1D
See WORLEY/2D
See WORLEY/3D
See VORONOI
See VORONOI-METHOD
See XXHASH (type)
See POINT (type)
See SAMPLE (type)"))

;; smooth-worley.lisp
(docs:define-docs
  (function smooth-worley/1d
    "Generate a 1D voronoi-smooth-worley noise sample.

This is a shorthand for using VORONOI/1D with the :WORLEY
VORONOI-METHOD.

See SMOOTH-WORLEY
See VORONOI
See VORONOI-METHOD
See XXHASH (type)
See POINT (type)
See SAMPLE (type)")
  
  (function smooth-worley/2d
    "Generate a 2D voronoi-smooth-worley noise sample.

This is a shorthand for using VORONOI/2D with the :SMOOTH-WORLEY
VORONOI-METHOD.

See SMOOTH-WORLEY
See VORONOI
See VORONOI-METHOD
See XXHASH (type)
See POINT (type)
See SAMPLE (type)")
  
  (function smooth-worley/3d
    "Generate a 3D voronoi-smooth-worley noise sample.

This is a shorthand for using VORONOI/3D with the :SMOOTH-WORLEY
VORONOI-METHOD.

See SMOOTH-WORLEY
See VORONOI
See VORONOI-METHOD
See XXHASH (type)
See POINT (type)
See SAMPLE (type)")
  
  (function smooth-worley
    "Generate a voronoi-smooth-worley noise sample.

This is a shorthand for using VORONOI with the :SMOOTH-WORLEY
VORONOI-METHOD.

See SMOOTH-WORLEY/1D
See SMOOTH-WORLEY/2D
See SMOOTH-WORLEY/3D
See VORONOI
See VORONOI-METHOD
See XXHASH (type)
See POINT (type)
See SAMPLE (type)"))

;; chebyshev.lisp
(docs:define-docs
  (function chebyshev/1d
    "Generate a 1D voronoi-chebyshev noise sample.

This is a shorthand for using VORONOI/1D with the :CHEBYSHEV
VORONOI-METHOD.

See CHEBYSHEV
See VORONOI
See VORONOI-METHOD
See XXHASH (type)
See POINT (type)
See SAMPLE (type)")
  
  (function chebyshev/2d
    "Generate a 2D voronoi-chebyshev noise sample.

This is a shorthand for using VORONOI/2D with the :CHEBYSHEV
VORONOI-METHOD.

See CHEBYSHEV
See VORONOI
See VORONOI-METHOD
See XXHASH (type)
See POINT (type)
See SAMPLE (type)")
  
  (function chebyshev/3d
    "Generate a 3D voronoi-chebyshev noise sample.

This is a shorthand for using VORONOI/3D with the :CHEBYSHEV
VORONOI-METHOD.

See CHEBYSHEV
See VORONOI
See VORONOI-METHOD
See XXHASH (type)
See POINT (type)
See SAMPLE (type)")
  
  (function chebyshev
    "Generate a voronoi-chebyshev noise sample.

This is a shorthand for using VORONOI with the :CHEBYSHEV
VORONOI-METHOD.

See CHEBYSHEV/1D
See CHEBYSHEV/2D
See CHEBYSHEV/3D
See VORONOI
See VORONOI-METHOD
See XXHASH (type)
See POINT (type)
See SAMPLE (type)"))

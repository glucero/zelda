#!/usr/bin/env ruby

## Gino Lucero
## github.com/glucero
## glucero@gmail.com

## A basic MIDI instrument and The Legend of Zelda Overworld Theme

# Example output (in mp3):  http://bit.ly/Qu6Efx
#
# Requirements:
#     Ruby 1.9, the Unimidi gem and a MIDI voice synthesizer
#     (if you're on OSX, you can use the Garage Band MIDI voices)

# Wikipedia's article on MIDI: http://bit.ly/1zuIwI
# Essentials of the MIDI protocol: http://bit.ly/P2UwPl
# The Legend of Zelda Sheet Music: http://bit.ly/N7tckt

require 'rubygems'
require 'unimidi'

C  = Bs = 0                   # define the pitch values
Cs = Df = 1                   #
D       = 2                   # (half of the MIDI message byte range is reserved)
Ds = Ef = 3                   # minimum: 0
E  = Ff = 4                   # maximum: 127
Es = F  = 5                   #
Fs = Gf = 6                   # the 's' means sharp
G       = 7                   # the 'f' means flat
Gs = Af = 8                   #
A       = 9                   # example:
As = Bf = 10                  #   Bf means 'B Flat'
B  = Cf = 11                  #   Fs means 'F Sharp'

OCTAVE = 12                   # 12 notes in the chromatic scale
R = nil                       # a musical rest. no signal pitch value

BPM = 140                     # beats per minute

module Meter

  # define note duration based on BPM.
  Q = (60 / BPM.to_f)         # quarter note:
                              #   number of seconds in a minute divided by BPM
  W = Q * 4                   # whole note:
                              #   duration of a quarter note multiplied by 4
  H = W / 2                   # half note:
                              #   duration of a whole note divided by 2

  E = Q / 2                   # eighth note
  S = E / 2                   # sixteenth note
  T = S / 2                   # thirty-second note

  DH = H + Q                  # dotted half note
  DQ = Q + E                  # dotted quarter note
  DE = E + S                  # dotted eighth note
  DS = S + T                  # dotted sixteenth note

  HT = W / 3                  # half note triplet
  QT = H / 3                  # quarter note triplet
  ET = Q / 3                  # eighth note triplet
  ST = E / 3                  # sixteenth note triplet

  # this allows 'on-the-fly' creation of single/double character methods.
  # it will use a numeric receiver as the note value (if it's is an array
  # of numbers, it will produce a chord) and will use one of the above
  # single/double character note types as the duration.
  #
  # example:
  #  D.q                      # D quarter note
  #  R.h                      # rest half note
  #  [C, E, G].w              # C major chord whole note
  #  [C, E, G].map(&:s)       # C major arpeggio sixteenth note sequence
  def method_missing(method, *opts, &block)
    if duration = method.to_s.match(/^(\w.?)$/)
      duration = duration[1].upcase

      if Meter.constants.include? duration.to_sym
        Note.new(duration, self)
      end
    end
  end

end

class Fixnum

  include Meter     # use numbers to represent pitches

  # this allows us to increase/decrease a Note's octave.
  #
  # example:
  #    D[2]         #=> 26
  #    C[:middle]   #=> 48
  #    A[-1]        #=> -3
  def [](octave)
    if octave.eql? :middle
      self + (OCTAVE * 4)     # the 4th C key on a piano
    else
      self + (OCTAVE * octave)
    end
  end
end


class NilClass

  include Meter     # use nil to represent rests

end

class Array

  include Meter     # use arrays of numbers to represent chords

end

module ArrayContainer

  attr_accessor :container

  # this allows us to easily call mutable array methods on classes without
  # having to sub-class 'Array'. dangerous, yes, but this is just a MIDI
  # sandbox and we're responsible adults.

  # ...right?
  def method_missing(method, *opts, &block)
    if @container.respond_to? method
      @container.send(method, *opts, &block)
    end
  end

end

class Note

  VOLUME = 100                # signal velocity (min: 0 - max: 100)

  ON  = 0x90                  # note on
  OFF = 0x80                  # note off

  include ArrayContainer

  attr_reader :duration
  attr_writer :voice

  # Notes need 3 things
  #   1. pitch (or pitches in the case of chords)
  #   2. duration
  #   3. voice (the instrument sound. piano/flute/trumpet/etc)
  # (the note's voice isn't required during creation and can be added later)
  def initialize(duration, pitches, voice = nil)
    @duration = Meter.const_get(duration)
    @container = [*pitches].flatten
    @voice = voice
  end

  # start note
  # sleep for the duration of the note
  # stop note
  def play
    start
    sleep duration
    stop
  end

  # start all pitches in the note container.
  def start
    @container.each do |note|
      @voice.puts(ON, note, VOLUME) unless note.nil?
    end
  end

  # stop all pitches in the note container.
  def stop
    @container.each do |note|
      @voice.puts(OFF, note, VOLUME) unless note.nil?
    end
  end

  # when cloning a note make sure to clone the container.
  def dup
    note = super
    note.container = note.container.dup
    note
  end

  # when changing a key's note, offset ALL pitches in the note's container.
  def key=(key)
    @container.map! { |pitch| pitch + key }
  end

end

class Sequence

  include ArrayContainer

  # *warning*
  # *warning*
  #   because we're using method_missing to call array methods on containers,
  #   flattening sequences that contain Notes will keep pitch values but lose
  #   all other Note information.
  # *warning*
  # *warning*

  def initialize
    @container = []

    yield self if block_given?
  end

  # Array#push allows multiple arguments and will let us append sequences
  # to the end of other sequences without having to worry about adding extra
  # dimensions
  def <<(sequence)
    @container.push *sequence
  end

  # sequence key changing allows two types of key changes:
  #
  # 1. a single digit
  #    the sequence will offset all notes (and their pitches) in the sequence
  #    by the same amount.
  #
  # 2. a sequence of numbers
  #    when using multiple numbers as the key change, each number in the key
  #    change should corespond with a number in the sequence. if the sequence
  #    of numbers is less than the number of notes you wish to offset, it will
  #    duplicate the key change sequence over and over until the number of
  #    key offsets is the same as the number of sequence notes.
  #
  #    example:
  #      sequence [C, E, G, C[1]]      (CMaj Arpeggio)
  #      key change G                  (offset by 7 half steps)
  #        => [G, B, D[1], G[1]]       (GMaj Arpeggio)
  #
  #    example:
  #      sequence [C, E, G, C[1]]      (CMaj Arpeggio)
  #      key change [E]
  #        => [E, Gs, B, E[1]]         (EMaj Arpeggio)
  #
  #    example:
  #      sequence [C, E, G, C[1]]      (CMaj Arpeggio)
  #      key change [0, -1, 0, 0]
  #        => [C, Ef, G, C[1]]         (Cm Arpeggio)
  #
  #    example:
  #      sequence [C, E, G, C[1]]      (CMaj Arpeggio)
  #      key change [0, -1, 0, 0]
  #        => [C, Ef, G, C[1]]         (Cm Arpeggio)
  def key=(key)
    key.push(*key.dup) until key.count >= self.count

    self.zip(key).map do |note,key|
      note.key = key
    end
  end

end

class Instrument

  attr_accessor :voice, :key, :song

  # creating an instrument requires key offset. by default, instruments will
  # use the first available MIDI voice (this can be changed after creation)
  #
  # example:
  #    violin = Instrument.new(C[5])
  #    viola  = Instrument.new(C[:middle])
  def initialize(key)
    @song = Sequence.new
    @key = key

    @voice = UniMIDI::Output.use(:first)
  end

  # you can add sequences, single notes or arrays of notes to an instrument.
  # example:
  #   instrument.add D[4].q
  #   instrument.add C.s, E.s, F.s
  #   instrument.add [F.e, A.e]
  #
  # per sequence key offsets help reuse similar patterns and must be passed
  # as blocks. these four examples produce the same result:
  #
  # scale_maj = [C, D, E, F, G, A, B, C[1]]
  # zeros = Array.new(8).fill(0)
  #
  # example:
  #   (adding the CMaj scale with no key offset)
  #   instrument.add scale_maj.map(&:s)
  #   instrument.play
  #
  # example:
  #   (adding the CMaj scale with a key offset of C (or 0)
  #   instrument.add(scale_maj.map(&:s)) { C }
  #   instrument.play
  #
  # example:
  #   (adding the CMaj scale with a sequence of 8 C notes as the key offset)
  #   instrument.add(scale_maj.map(&:s)) { zeros }
  #   instrument.play
  #
  # example:
  #   (adding a sequence of 8 C notes with a CMaj scale as the key offset)
  #   instrument.add(zeros.map(&:s)) { scale_maj }
  #   instrument.play
  def add(*notes)
    @song << Sequence.new do |sequence|

      notes = notes.shift if notes.first.is_a? Array

      sequence << notes.map do |note|
        note = note.dup
        note.key = @key
        note.voice = @voice
        note
      end

      sequence.key = yield if block_given?
    end
  end

  # clear all notes from the instrument's 'song' container
  def clear
    @song.clear
  end

  # play all notes in the instrument's 'song' container in sequential order
  def play
    @song.map(&:play)
  end

end

# create the three instruments needed
melody = Instrument.new C[5]
harmony = Instrument.new C[:middle]
counter_melody = Instrument.new C[3]

# intro

melody.add Bf.h, R.de, Bf.s, Bf.et, Bf.et, Bf.et
harmony.add D[1].h, R.de, D[1].s, D[1].et, D[1].et, D[1].et
counter_melody.add Bf.q, Bf.et, Bf.et, Bf.et, Bf.q, Bf.et, Bf.et, Bf.et

melody.add Bf.de, Af.s, Bf.dq, Bf.s, Bf.s, Bf.et, Bf.et, Bf.et
harmony.add C[1].de, C[1].s, C[1].h, C[1].et, C[1].et, C[1].et
counter_melody.add Af.q, Af.et, Af.et, Af.et, Af.q, Af.et, Af.et, Af.et

melody.add Bf.de, Af.s, Bf.dq, Bf.s, Bf.s, Bf.et, Bf.et, Bf.et
harmony.add Df[1].de, Df[1].s, Df[1].dq, Df[1].s, Df[1].s, Df[1].et, Df[1].et, Df[1].et
counter_melody.add Gf.q, Gf.et, Gf.et, Gf.et, Gf.q, Gf.et, Gf.et, Gf.et

melody.add Bf.e, F.s, F.s, F.e, F.s, F.s, F.e, F.s, F.s, F.e, F.e
harmony.add Df[1].e, Af.s, Af.s, Af.e, Af.s, Af.s, Af.e, Af.s, Af.s, Af.e, Af.e
counter_melody.add Gf.q, F.q, F.q, G.e, A.e

# main theme (played twice)
melody.add Bf.q, F.dq, F.s, Bf.s, Bf.s, C[1].s, D[1].s, Ef[1].s
harmony.add D[1].q, D[1].et, D[1].et, C[1].et, D[1].de, D[1].s, D[1].s, Ef[1].s, F[1].s, G[1].s
counter_melody.add Bf.q, Bf.et, Bf.et, Af.et, Bf.q, Bf.q

melody.add F[1].h, R.e, F[1].e, F[1].et, Gf[1].et, Af[1].et
harmony.add Af[1].de, Bf[1].s, Bf[1].s, C[2].s, D[2].s, Ef[2].s, F[2].e, F[2].e, Af[1].et, Bf[1].et, C[2].et
counter_melody.add Af.q, Af.et, Af.et, Gf.et, Af.q, Af.q

melody.add Bf[1].h, Bf[1].ds, Bf[1].ds, Bf[1].s, Bf[1].et, Af[1].et, Gf[1].et
harmony.add Df[1].de, Gf.s, Gf.s, Af.s, Bf.s, C[1].s, Df[1].e, Df[1].e, Df[1].et, C[1].et, Bf.et
counter_melody.add Gf.q, Gf.et, Gf.et, E.et, Gf.q, Gf.q

melody.add Af[1].de, Gf[1].s, F[1].h, F[1].q
harmony.add Df[1].de, Af.s, Af.et, Af.et, Gf.et, Af.de, Af.s, Af.et, Gf.et, Af.et
counter_melody.add Df[1].q, Df[1].et, Df[1].et, B.et, Df[1].q, Df[1].q

melody.add Ef[1].e, Ef[1].s, F[1].s, Gf[1].h, F[1].e, Ef[1].e
harmony.add Gf.e, Gf.st, Gf.st, F.st, Gf.e, Gf.st, Gf.st, Af.st, Bf.q, Af.e, Gf.e
counter_melody.add B.q, B.et, B.et, Bf.et, B.q, B.et, B.et, B.et

melody.add Df[1].e, Df[1].s, Ef[1].s, F[1].h, Ef[1].e, Df[1].e
harmony.add F.e, F.st, F.st, Ef.st, F.e, F.st, F.st, Gf.st, Af.q, Gf.e, F.e
counter_melody.add Bf.q, Bf.et, Bf.et, Af.et, Bf.q, Bf.et, Bf.et, Bf.et

melody.add C[1].e, C[1].s, D[1].s, E.h, G.q
harmony.add E.q, E.de, F.s, G.e, G.st, G.st, A.st, Bf.e, C.e
counter_melody.add C.q, C.et, C.et, Bf.et, C.q, C.et, C.et, C.et

melody.add F[1].e, F.s, F.s, F.e, F.s, F.s, F.e, F.s, F.s, F.e, F.e
harmony.add A.e, C.s, C.s, C.e, C.s, C.s, C.e, C.s, C.s, C.e, C.e
counter_melody.add F.q, F.q, F.q, G.e, A.e

# main part 2

melody.add Bf.q, F.dq, F.s, Bf.s, Bf.s, C[1].s, D[1].s, Ef[1].s
harmony.add D[1].q, D[1].et, D[1].et, C[1].et, D[1].de, D[1].s, D[1].s, Ef[1].s, F[1].s, G[1].s
counter_melody.add Bf.q, Bf.et, Bf.et, Af.et, Bf.q, Bf.q

melody.add F[1].h, R.e, F[1].e, F[1].et, Gf[1].et, Af[1].et
harmony.add Af[1].de, Bf[1].s, Bf[1].s, C[2].s, D[2].s, Ef[2].s, F[2].e, F[2].e, Af[1].et, Bf[1].et, C[2].et
counter_melody.add Af.q, Af.et, Af.et, Gf.et, Af.q, Af.q

melody.add Bf[1].dh, Df[2].q
harmony.add Df[2].dh, E[2].q
counter_melody.add Gf.q, Gf.et, Gf.et, E.et, Gf.q, Gf.q

melody.add C[2].q, A[1].h, F[1].q
harmony.add E[2].q, C[2].h, A[1].q
counter_melody.add F.q, F.et, F.et, Ef.et, F.q, F.q

melody.add Gf[1].dh, Bf[1].q
harmony.add B[1].dh, Df[2].q
counter_melody.add E.et, Bf.et, Df[1].et, E[1].et, Bf[2].et, Df[3].et, E[3].q, R.q

melody.add A[1].q, F[1].h, F[1].q
harmony.add C[2].q, A[1].h, A[1].q
counter_melody.add F[2].q, F.et, F.et, F.et, F.q, R.q

melody.add Gf[1].h, R.q, Bf[1].q
harmony.add B[1].h, R.q, Df[2].q
counter_melody.add E.et, Bf.et, Df[1].et, E[1].et, Bf[2].et, Df[3].et, E[3].q, R.q

melody.add A[1].q, F[1].h, D[1].q
harmony.add C[2].q, A[1].h, A[1].q
counter_melody.add F[2].q, F.et, F.et, F.et, F.q, R.q

melody.add Ef[1].h, R.q, Gf[1].q
harmony.add Gf[1].h, R.q, B[1].q
counter_melody.add B.q, B.et, B.et, Bf.et, B.q, B.et, B.et, B.et

melody.add F[1].q, Df[1].h, Bf.q
harmony.add Bf[1].q, F[1].h, Df[1].q
counter_melody.add Bf.q, Bf.et, Bf.et, Af.et, Bf.q, Bf.et, Bf.et, Bf.et

melody.add C[1].e, C[1].s, D[1].s, E[1].h, G[1].q
harmony.add E[1].q, E[1].de, F[1].s, G[1].e, G[1].st, G[1].st, A[1].st, Bf[1].e, C[2].e
counter_melody.add C[1].q, C[1].et, C[1].et, C[1].et, C[1].q, C[1].et, C[1].et, C[1].et

melody.add F[1].e, F.s, F.s, F.e, F.s, F.s, F.e, F.s, F.s, F.e, F.e
harmony.add A[1].e, A.s, A.s, A.e, A.s, A.s, A.e, A.s, A.s, A.e, A.e
counter_melody.add F[1].q, F[1].q, F[1].q, G[1].e, A[1].e

#######
# gather all three instruments and play them at the same time
trio = [melody, harmony, counter_melody]

time = Time.now.to_i + 5

trio.map do |instrument|
  Thread.new do |t|
    t.priority = 99
    loop do
      # sync starting of threads to five seconds from now
      # in case we're not done loading everything.
      if Time.now.to_i.eql? time
        instrument.play and break
      end
    end
  end
end.map(&:join)


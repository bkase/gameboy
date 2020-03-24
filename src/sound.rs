use mem::Memory;
use packed_struct::prelude::*;
use read_view_u8::*;

//          4       3         2         1        0
// PulseA  Control Frequency  Volume  Length   Sweep
//

#[derive(PackedStruct, Debug, PartialEq)]
#[packed_struct(size_bytes = "1", bit_numbering = "lsb0")]
pub struct Control {
    #[packed_field(bits = "7")]
    pub trigger: bool,
    #[packed_field(bits = "6")]
    pub counter_consecutive: bool,
    #[packed_field(bits = "3:5")]
    pub null: Integer<u8, packed_bits::Bits3>,
    #[packed_field(bits = "0:2")]
    pub frequency_high_bits: Integer<u8, packed_bits::Bits3>,
}
impl ReadViewU8 for Control {
    fn read(&self) -> u8 {
        self.pack()[0]
    }
}
impl ViewU8 for Control {
    fn set(&mut self, n: u8) {
        *self = Control::unpack(&[n]).expect("it's 8bits")
    }
}

#[derive(Debug, PartialEq)]
pub struct FrequencyLowBits(u8);
impl ReadViewU8 for FrequencyLowBits {
    fn read(&self) -> u8 {
        self.0
    }
}
impl ViewU8 for FrequencyLowBits {
    fn set(&mut self, n: u8) {
        *self = FrequencyLowBits(n)
    }
}

#[derive(PackedStruct, Debug, PartialEq)]
#[packed_struct(size_bytes = "1", bit_numbering = "lsb0")]
pub struct Volume {
    #[packed_field(bits = "4:7")]
    pub initial_volume: Integer<u8, packed_bits::Bits4>,

    // false = Attenuate
    // true = Amplify
    #[packed_field(bits = "3")]
    pub envelope_up_down: bool,

    #[packed_field(bits = "0:2")]
    pub envelope_sweep: Integer<u8, packed_bits::Bits3>,
}
impl ReadViewU8 for Volume {
    fn read(&self) -> u8 {
        self.pack()[0]
    }
}
impl ViewU8 for Volume {
    fn set(&mut self, n: u8) {
        *self = Volume::unpack(&[n]).expect("it's 8bits")
    }
}

#[derive(PackedStruct, Debug, PartialEq)]
#[packed_struct(size_bytes = "1", bit_numbering = "lsb0")]
pub struct Length {
    // 00 = 12.5
    // 01 = 25
    // 10 = 50
    // 11 = 75
    #[packed_field(bits = "6:7")]
    pub wave_duty: Integer<u8, packed_bits::Bits2>,

    // (64-x)*(1/256) seconds
    #[packed_field(bits = "0:5")]
    pub sound_length: Integer<u8, packed_bits::Bits6>,
}
impl ReadViewU8 for Length {
    fn read(&self) -> u8 {
        self.pack()[0]
    }
}
impl ViewU8 for Length {
    fn set(&mut self, n: u8) {
        *self = Length::unpack(&[n]).expect("it's 8bits")
    }
}

#[derive(PackedStruct, Debug, PartialEq)]
#[packed_struct(size_bytes = "1", bit_numbering = "lsb0")]
pub struct Sweep {
    #[packed_field(bits = "7")]
    pub null: bool,

    // Sweep Time:
    // 000: sweep off - no freq change
    // 001: 7.8 ms (1/128Hz)
    // 010: 15.6 ms (2/128Hz)
    // 011: 23.4 ms (3/128Hz)
    // 100: 31.3 ms (4/128Hz)
    // 101: 39.1 ms (5/128Hz)
    // 110: 46.9 ms (6/128Hz)
    // 111: 54.7 ms (7/128Hz)
    #[packed_field(bits = "4:6")]
    pub sweep_time: Integer<u8, packed_bits::Bits3>,

    // false = Addition (freq increase)
    // true = Subtraction (freq decrease)
    #[packed_field(bits = "3")]
    pub sweep_direction: bool,

    #[packed_field(bits = "0:2")]
    pub sweep_shift_number: Integer<u8, packed_bits::Bits3>,
}
impl ReadViewU8 for Sweep {
    fn read(&self) -> u8 {
        self.pack()[0]
    }
}
impl ViewU8 for Sweep {
    fn set(&mut self, n: u8) {
        *self = Sweep::unpack(&[n]).expect("it's 8bits")
    }
}

#[derive(Debug, PartialEq)]
pub struct PulseA {
    pub control: Control,
    pub frequency: FrequencyLowBits,
    pub volume: Volume,
    pub length: Length,
    pub sweep: Sweep,
}
impl PulseA {
    pub fn create() -> PulseA {
        PulseA {
            control: Control::unpack(&[0x00]).expect("Fits within a u8"),
            frequency: FrequencyLowBits(0),
            volume: Volume::unpack(&[0x00]).expect("Fits within a u8"),
            length: Length::unpack(&[0x00]).expect("Fits within a u8"),
            sweep: Sweep::unpack(&[0x00]).expect("Fits within a u8"),
        }
    }
}

#[derive(PackedStruct, Debug, PartialEq)]
#[packed_struct(size_bytes = "1", bit_numbering = "lsb0")]
pub struct SoundOnOff {
    // 0 = off
    // 1 = on
    #[packed_field(bits = "7")]
    pub all: bool,

    #[packed_field(bits = "4:6")]
    null: Integer<u8, packed_bits::Bits3>,

    #[packed_field(bits = "3")]
    pub sound4: bool,
    #[packed_field(bits = "2")]
    pub sound3: bool,
    #[packed_field(bits = "1")]
    pub sound2: bool,
    #[packed_field(bits = "0")]
    pub sound1: bool,
}
impl ReadViewU8 for SoundOnOff {
    fn read(&self) -> u8 {
        self.pack()[0]
    }
}
impl ViewU8 for SoundOnOff {
    fn set(&mut self, n: u8) {
        *self = SoundOnOff::unpack(&[n]).expect("it's 8bits")
    }
}

#[derive(Debug, PartialEq)]
pub struct Registers {
    pub pulse_a: PulseA,
    pub sound_on_off: SoundOnOff,
}
impl Registers {
    pub fn create() -> Registers {
        Registers {
            pulse_a: PulseA::create(),
            sound_on_off: SoundOnOff::unpack(&[0x00]).expect("it's 8bits"),
        }
    }
}

//
//
//     Time ->
//  n=3
//
//   ON----3/64sec----
// VOL=$F----$E-----$D
//

#[derive(Debug, PartialEq)]
pub enum PulseKind {
    FirstEigth,   // 00
    FirstQuarter, // 01
    FiftyPercent, // 10
    LastQuarter,  // 11
}
impl PulseKind {
    fn of_integer2(x: Integer<u8, packed_bits::Bits2>) -> PulseKind {
        match x.into() {
            0 => PulseKind::FirstEigth,
            1 => PulseKind::FirstQuarter,
            2 => PulseKind::FiftyPercent,
            3 => PulseKind::LastQuarter,
            _ => panic!("Impossible"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Oscillator {
    Pulse(PulseKind),
}

const GAIN_EPSLION: f32 = 0.000_000_01;
#[derive(Debug, PartialEq)]
pub struct Channel {
    pub gain: f32,
    pub oscillator: Oscillator,
    pub frequency: f32,
}

const ONE_SIXTY_FOURTH_SECS_IN_TICKS: u32 = 16384;
#[derive(Debug, PartialEq)]
pub enum Behavior {
    Decaying(u32), // decrease gain by one unit every u32 ticks
}

#[derive(Debug, PartialEq)]
pub struct Audio {
    pub channel: Channel,
    pub behavior: Behavior,
    pub ticks_passed: u32,
}

#[derive(Debug, PartialEq)]
pub struct Sound {
    pub dirty: bool,
    // TODO: Support multiple channels
    // the u32 is ticks passed
    pub audio: Option<Audio>,
}

impl Sound {
    pub fn create() -> Sound {
        Sound {
            dirty: true,
            audio: None,
        }
    }

    fn frequency_of_gb_frequency(pulse_a: &PulseA) -> f32 {
        let high_bits: u8 = pulse_a.control.frequency_high_bits.into();
        let low_bits: u16 = u16::from(pulse_a.frequency.0);
        let raw_gb_frequency = (u16::from(high_bits) << 8) | low_bits;
        // max 11bits
        assert!(raw_gb_frequency < 2048);
        // from GBCPUMan
        131_072.0 / f32::from(2048 - raw_gb_frequency)
    }

    pub fn advance(&mut self, memory: &mut Memory, duration: u32) {
        let pulse_a_on = memory.sound.pulse_a.control.trigger;

        let mut should_drop = false;
        match &mut self.audio {
            None => (),
            Some(audio) => {
                audio.channel.frequency = Self::frequency_of_gb_frequency(&memory.sound.pulse_a);
                match audio.behavior {
                    Behavior::Decaying(ticks) => {
                        assert!(audio.ticks_passed < ticks);
                        if audio.ticks_passed + duration > ticks {
                            audio.ticks_passed = (audio.ticks_passed + duration) % ticks;
                            audio.channel.gain -= 1.0 / 64.0;
                            if audio.channel.gain < GAIN_EPSLION
                                || -1.0 * audio.channel.gain > -1. * GAIN_EPSLION
                            {
                                audio.channel.gain = 0.0;
                                // DROP the audio here once gain hits zero (we're done)
                                should_drop = true;
                            }
                        } else {
                            audio.ticks_passed = (audio.ticks_passed + duration) % ticks;
                        }
                    }
                }
            }
        }
        if should_drop {
            self.audio = None;
        }

        // TODO: Clean up this rising-edge logic
        if pulse_a_on && self.dirty {
            self.dirty = false;

            self.audio = Some(Audio {
                channel: Channel {
                    gain: {
                        let initial: u8 = memory.sound.pulse_a.volume.initial_volume.into();
                        f32::from(initial) / 64.0
                    },
                    oscillator: Oscillator::Pulse(PulseKind::of_integer2(
                        memory.sound.pulse_a.length.wave_duty,
                    )),
                    frequency: Self::frequency_of_gb_frequency(&memory.sound.pulse_a),
                },
                // TODO: Use volume info and length to determine behavior
                behavior: Behavior::Decaying({
                    let mut sweep: u8 = memory.sound.pulse_a.volume.envelope_sweep.into();
                    if !(sweep < 16 && sweep > 0) {
                        use web_utils::log;
                        log(&format!(
                            "WARNING: sweep is supposed to be between 0 and 16 but was {:}",
                            sweep,
                        ));
                        sweep = 1;
                    }
                    ONE_SIXTY_FOURTH_SECS_IN_TICKS * u32::from(sweep)
                }),
                ticks_passed: 0,
            });
        } else {
            self.dirty = !pulse_a_on;
        }
    }
}

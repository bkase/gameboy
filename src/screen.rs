pub struct Screen {
    pub data: Vec<u8>,
    _width: u32,
    _height: u32,
}

#[derive(Debug, Clone, Copy)]
pub struct Rgb {
    pub r: u8,
    pub g: u8,
    pub b: u8,
}

#[derive(Debug, Clone, Copy)]
pub struct Coordinate {
    pub x: u8,
    pub y: u8,
}

impl Screen {
    pub fn create(width: u32, height: u32) -> Screen {
        Screen {
            data: vec![0; (height * width * 4) as usize],
            _width: width,
            _height: height,
        }
    }

    #[inline]
    pub fn bang(&mut self, rgb: Rgb, coord: Coordinate) {
        let idx: usize = (coord.x * coord.y * 4 + coord.y * 4) as usize;
        self.data[idx] = rgb.r;
        self.data[idx + 1] = rgb.g;
        self.data[idx + 2] = rgb.b;
        self.data[idx + 3] = 255;
    }
}

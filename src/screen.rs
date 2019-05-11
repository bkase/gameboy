use web_utils::log;

pub struct Screen {
    pub data: Vec<u8>,
    width: u32,
    height: u32,
}

#[derive(Debug, Clone, Copy, PartialEq)]
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
            width,
            height,
        }
    }

    #[inline]
    pub fn bang(&mut self, rgb: Rgb, coord: Coordinate) {
        // Total size = height * width * 4
        //
        // (y * width * 4) + (x * 4)
        // assert y < height && x < width

        assert!((coord.y as u32) < self.height);
        assert!((coord.x as u32) < self.width);

        let idx: usize = ((coord.y as u32) * self.width * 4 + (coord.x as u32) * 4) as usize;
        self.data[idx] = rgb.r;
        self.data[idx + 1] = rgb.g;
        self.data[idx + 2] = rgb.b;
        self.data[idx + 3] = 255;
    }
}

pub struct Color(u8, u8, u8);

impl Color {
    pub fn from_rgb(rgb: &str) -> Result<Color, &'static str> {
        let parts: Vec<&str> = rgb.split(',').collect();
        if parts.len() == 3 {
            let r = parts[0].parse::<u8>().map_err(|_| "Invalid RGB format")?;
            let g = parts[1].parse::<u8>().map_err(|_| "Invalid RGB format")?;
            let b = parts[2].parse::<u8>().map_err(|_| "Invalid RGB format")?;
            Ok(Color(r, g, b))
        } else {
            Err("Invalid RGB format")
        }
    }

    pub fn from_hex(hex: &str) -> Result<Color, &'static str> {
        if hex.len() != 7 || !hex.starts_with('#') {
            return Err("Invalid HEX format");
        }

        let r = u8::from_str_radix(&hex[1..3], 16).map_err(|_| "Invalid HEX value")?;
        let g = u8::from_str_radix(&hex[3..5], 16).map_err(|_| "Invalid HEX value")?;
        let b = u8::from_str_radix(&hex[5..7], 16).map_err(|_| "Invalid HEX value")?;

        Ok(Color(r, g, b))
    }
}

pub trait Colorize {
    fn color(self, color: &str) -> String;
    fn bg_color(self, color: &str) -> String;
    fn bold(self) -> String;
    fn italic(self) -> String;
    fn underline(self) -> String;
    fn strikethrough(self) -> String;
    fn dim(self) -> String;
    fn invert(self) -> String;
}

impl Colorize for &str {
    fn color(self, color: &str) -> String {
        let color = if color.starts_with('#') {
            Color::from_hex(color)
        } else {
            Color::from_rgb(color)
        };

        match color {
            Ok(c) => format!("\x1b[38;2;{};{};{}m{}\x1b[0m", c.0, c.1, c.2, self),
            Err(_) => self.to_string(),
        }
    }

    fn bg_color(self, color: &str) -> String {
        let color = if color.starts_with('#') {
            Color::from_hex(color)
        } else {
            Color::from_rgb(color)
        };

        match color {
            Ok(c) => format!("\x1b[48;2;{};{};{}m{}\x1b[0m", c.0, c.1, c.2, self),
            Err(_) => self.to_string(),
        }
    }

    fn bold(self) -> String {
        format!("\x1b[1m{}\x1b[0m", self)
    }

    fn italic(self) -> String {
        format!("\x1b[3m{}\x1b[0m", self)
    }

    fn underline(self) -> String {
        format!("\x1b[4m{}\x1b[0m", self)
    }

    fn strikethrough(self) -> String {
        format!("\x1b[9m{}\x1b[0m", self)
    }

    fn dim(self) -> String {
        format!("\x1b[2m{}\x1b[0m", self)
    }

    fn invert(self) -> String {
        format!("\x1b[7m{}\x1b[0m", self)
    }
}



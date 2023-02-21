pub struct SourceProcess {
    pub name: &'static str,
    pub val: i32
}

impl SourceProcess {
    pub fn spawn(name: &'static str, val: i32) -> SourceProcess {
        SourceProcess {
            name,
            val,
        }
    }
}

impl Actor for SourceProcess {
    type Stdout = i32;
    type Stdin = ();
    fn process(&self, _item: Self::Stdin) -> Result<Self::Stdout, Stderr> {
        println!("This is {}, output is {}", self.name, self.val);
        Ok(self.val)
    }
}

pub struct AddProcess {
    pub name: &'static str,
}

impl AddProcess {
    pub fn spawn(name: &'static str) -> AddProcess {
        AddProcess {
            name,
        }
    }
}

impl Actor for AddProcess {
    type Stdout = i32;
    type Stdin = i32;
    fn process(&self, item: Self::Stdin) -> Result<Self::Stdout, Stderr> {
        let output = item + 1;
        println!("This is {}, item is {}, output is {}", self.name, item, output);
        Ok(output)
    }
}

pub struct SinkProcess {
    pub name: &'static str,
}

impl SinkProcess {
    pub fn spawn(name: &'static str) -> SinkProcess {
        SinkProcess {
            name,
        }
    }
}

impl Actor for SinkProcess {
    type Stdout = ();
    type Stdin = i32;
    fn process(&self, item: Self::Stdin) -> Result<Self::Stdout, Stderr> {
        println!("This is {}, result is {}", self.name, item);

        Ok(())
    }
}

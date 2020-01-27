#![feature(proc_macro_hygiene)]
#![cfg(test)]

extern crate command_macros_plugin;
extern crate duct;

macro_rules! command_test_suite {
    ($invoke:ident, $quicktest:ident) => {
        #[test]
        fn good() {
            let bar = false;
            let option = Some(5);
            $quicktest(
                $invoke!(echo {} if bar {-=bar=-} else if let Some(a) = option {--number (a.to_string())} levano),
                "{} --number 5 levano"
            );
            let bar = true;
            $quicktest(
                $invoke!(echo {} if bar {-=bar=-} else if let Some(a) = option {--number (a.to_string())} levano),
                "{} -=bar=- levano"
            );
        }

        #[test]
        fn ffmpeg() {
            let moreargs = ["-pix_fmt", "yuv420p"];
            let file = "file.mp4".to_string();
            let preset = "slow";
            let tmpname = "tmp.mkv";
            $quicktest(
                $invoke!(
                    echo -i (file)
                    -c:v libx264 -preset (preset) [&moreargs]
                    -c:a copy
                    file:(tmpname)
                ),
                "-i file.mp4 -c:v libx264 -preset slow -pix_fmt yuv420p -c:a copy file:tmp.mkv"
            );
        }

        #[test]
        fn strings() {
            $quicktest($invoke!("echo" r"a~\b"), "a~\\b");
        }

        #[test]
        fn ugly() {
            $quicktest($invoke!(echo if {{}; false}{a}else{b}), "b");
            // $quicktest($invoke!(echo if-a=5 {} else {}), "if-a=5 {} else {}");
            // $quicktest($invoke!(echo else-a {} let-a {}), "else-a {} let-a {}");
        }

        #[test]
        fn match_test() {
            for &(x, target) in &[
                (Ok(1), ". 0101 ."),
                (Ok(5), ". small 5 ."),
                (Ok(10), ". 10 ."),
                (Err("bu"), ". err bu ."),
            ] {
                $quicktest($invoke!(
                        echo . match x {
                            Ok(0) | Ok(1) => { 0101 },
                            Ok(x) if x < 7 => { small (x.to_string()) },
                            Ok(x) => { (x.to_string()) }
                            Err(x) => { err (x) }
                        } .
                    ),
                    target
                );
            }
        }

        #[test]
        fn parenparen() {
            $quicktest($invoke!(echo ((2+2))), "4");
            fn inc(x: i32) -> String { (x + 1).to_string() };
            $quicktest($invoke!(echo ((inc)(3))), "4");
        }

        #[test]
        fn touching() {
            $quicktest($invoke![echo number((2+2))], "number4");
            $quicktest($invoke![("e")"ch"(('o')) number((2+2))], "number4");
            $quicktest($invoke![echo ("abc")-((5))-def.txt hij], "abc-5-def.txt hij");
        }

        #[test]
        fn for_loop() {
            $quicktest($invoke![
                    echo
                    for (i, x) in ["a", "b"].iter().enumerate() {
                        --add ((i+1)).(x).txt
                    }
                    end
                ],
                "--add 1.a.txt --add 2.b.txt end"
            );
            // $quicktest($invoke!(echo for-me), "for-me");
        }

        #[test]
        fn hygiene() {
            let cmd = 42;
            $quicktest($invoke![echo ((cmd))], "42");
        }

        #[test]
        fn flags_warn() {
            // $quicktest($invoke![echo . (--flag) (+c)], ". --flag +c");
        }

        #[test]
        fn quoted() {
            $quicktest($invoke!(echo "foo" "bar"), "foo bar");
            $quicktest($invoke!(echo "\"foo\" bar"), "\"foo\" bar");
            $quicktest($invoke!(echo "\"foo\"" ("bar baz")), "\"foo\" bar baz");
        }

        #[test]
        fn extend_command() {
            let base_cmd = $invoke!(echo foo);

            $quicktest($invoke!({base_cmd} bar), "foo bar");
        }
    };
}

mod command {
    use std::process::Command;
    use command_macros_plugin::command;

    fn quicktest(mut echocmd: Command, target: &str) {
        let out = echocmd.output().expect("quicktest: can't echo").stdout;
        assert_eq!(String::from_utf8_lossy(&out).trim(), target);
    }

    command_test_suite!(command, quicktest);

    #[test]
    fn not_moving() {
        let s = String::new();
        command!((s));
        command!(((s)));
        command!((s));
    }
}

mod duct_command {
    use command_macros_plugin::duct_command;
    use duct::Expression;

    fn quicktest(echocmd: Expression, target: &str) {
        let out = echocmd.read().expect("quicktest: can't echo");
        assert_eq!(out.trim(), target);
    }

    command_test_suite!(duct_command, quicktest);
}

mod command_args {
    use command_macros_plugin::command_args;

    #[test]
    fn good() {
        let bar = false;
        let option = Some(5);
        assert_eq!(
            command_args!({} if bar {-=bar=-} else if let Some(a) = option {--number (a.to_string())} levano),
            &["{}", "--number", "5", "levano"]
        );
        let bar = true;
        assert_eq!(
            command_args!({} if bar {-=bar=-} else if let Some(a) = option {--number (a.to_string())} levano),
            &["{}", "-=bar=-", "levano"]
        );
    }

    #[test]
    fn ffmpeg() {
        let moreargs = ["-pix_fmt", "yuv420p"];
        let file = "file.mp4".to_string();
        let preset = "slow";
        let tmpname = "tmp.mkv";
        assert_eq!(
            command_args!(
                -i (file)
                -c:v libx264 -preset (preset) [&moreargs]
                -c:a copy
                file:(tmpname)
            ),
            &["-i", "file.mp4", "-c:v", "libx264", "-preset", "slow", "-pix_fmt", "yuv420p", "-c:a", "copy", "file:tmp.mkv"]
        );
    }

    #[test]
    fn strings() {
        assert_eq!(command_args!(r"a~\b"), &["a~\\b"]);
    }

    #[test]
    fn ugly() {
        assert_eq!(command_args!(if {{}; false}{a}else{b}), &["b"]);
    }

    #[test]
    fn match_test() {
        for &(x, target) in &[
            (Ok(1), &[".", "0101", "."] as &[&str]),
            (Ok(5), &[".",  "small", "5", "."]),
            (Ok(10), &[".", "10", "."]),
            (Err("bu"), &[".", "err", "bu", "."]),
        ] {
            assert_eq!(command_args!(
                    . match x {
                        Ok(0) | Ok(1) => { 0101 },
                        Ok(x) if x < 7 => { small (x.to_string()) },
                        Ok(x) => { (x.to_string()) }
                        Err(x) => { err (x) }
                    } .
                ),
                target
            );
        }
    }

    #[test]
    fn parenparen() {
        assert_eq!(command_args!(((2+2))), &["4"]);
        fn inc(x: i32) -> String { (x + 1).to_string() };
        assert_eq!(command_args!(((inc)(3))), &["4"]);
    }

    #[test]
    fn touching() {
        assert_eq!(command_args![number((2+2))], &["number4"]);
        assert_eq!(command_args![("abc")-((5))-def.txt hij], &["abc-5-def.txt", "hij"]);
    }

    #[test]
    fn for_loop() {
        assert_eq!(command_args![
                for (i, x) in ["a", "b"].iter().enumerate() {
                    --add ((i+1)).(x).txt
                }
                end
            ],
            &["--add", "1.a.txt", "--add", "2.b.txt", "end"]
        );
    }

    #[test]
    fn hygiene() {
        let cmd = 42;
        assert_eq!(command_args![((cmd))], &["42"]);
    }

    #[test]
    fn quoted() {
        assert_eq!(command_args!("foo" "bar"), &["foo", "bar"]);
        assert_eq!(command_args!("\"foo\" bar"), &["\"foo\" bar"]);
        assert_eq!(command_args!("\"foo\"" ("bar baz")), &["\"foo\"", "bar baz"]);
    }
}

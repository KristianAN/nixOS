{ ... }:
{
  programs.yambar = {
    enable = true;
    settings = {
      bar = {
        location = "top";
        height = 26;
        background = "16161DFF";
        foreground = "C8C093FF";

        font = "monospace:pixelsize=16";
        spacing = 6;
        margin = 12;
        left = [
          {
            i3 = {
              sort = "ascending";
              content."".map =
                let
                  default = {
                    text = "{name}";
                    margin = 6;
                    on-click = "swaymsg --quiet workspace {name}";
                  };
                  focused.foreground = "957FB8FF";
                  urgent = {
                    foreground = "E82424FF";
                    deco.stack = [ { background.color = "BF616AFF"; } ];
                  };
                  underline.underline = {
                    size = 2;
                    color = "81A1C1FF";
                  };
                in
                {
                  default.string = default;
                  conditions = {
                    "state == focused".string = default // focused;
                    "state == urgent".string = default // urgent;
                  };
                };
            };
          }
        ];
        center = [
          {
            clock = {
              date-format = "%a %m-%d";
              content = [
                {
                  string = {
                    text = "";
                    right-margin = 4;
                  };
                }
                { string.text = "{date} {time}"; }
              ];
            };
          }
        ];
        right = [
          {
            cpu = {
              poll-interval = 10000;
              content.map.conditions."id < 0" = [
                {
                  string = {
                    text = "󰓅";
                    right-margin = 4;
                  };
                }
                { string.text = "{cpu}%"; }
              ];
            };
          }
          {
            mem = {
              poll-interval = 10000;
              content = [
                {
                  string = {
                    text = "󰍛";
                    right-margin = 4;
                  };
                }
                { string.text = "{free:gb}G"; }
              ];
            };
          }
          {
            pulse.content = [
              {
                map.conditions = {
                  sink_muted.string = {
                    text = "󰖁";
                    right-margin = 4;
                  };
                  "~sink_muted".string = {
                    text = "󰕾";
                    right-margin = 4;
                  };
                };
              }
              { string.text = "{sink_percent}"; }
            ];
          }
          {
            battery = {
              name = "BAT0";
              poll-interval = 30000;

              content = {
                map = {
                  default = [
                    {
                      ramp = {
                        tag = "capacity";
                        items = [
                          {
                            string = {
                              text = "󰁺";
                              foreground = "ff0000ff";
                            };
                          }
                          {
                            string = {
                              text = "󰁻";
                              foreground = "ffa600ff";
                            };
                          }
                          {
                            string = {
                              text = "󰁼";
                            };
                          }
                          {
                            string = {
                              text = "󰁽";
                            };
                          }
                          {
                            string = {
                              text = "󰁾";
                            };
                          }
                          {
                            string = {
                              text = "󰁿";
                            };
                          }
                          {
                            string = {
                              text = "󰂀";
                            };
                          }
                          {
                            string = {
                              text = "󰂁";
                            };
                          }
                          {
                            string = {
                              text = "󰂂";
                            };
                          }
                          {
                            string = {
                              text = "󰁹";
                              foreground = "00ff00ff";
                            };
                          }
                        ];
                      };
                    }
                    {
                      string = {
                        text = " {capacity:02}% {estimate}";
                      };
                    }
                  ];
                  conditions = {
                    "state == charging" = [
                      {
                        ramp = {
                          tag = "capacity";
                          foreground = "98BB6Cff";
                          items = [
                            {
                              string = {
                                text = "󰢜";
                              };
                            }
                            {
                              string = {
                                text = "󰂆";
                              };
                            }
                            {
                              string = {
                                text = "󰂇";
                              };
                            }
                            {
                              string = {
                                text = "󰂈";
                              };
                            }
                            {
                              string = {
                                text = "󰢝";
                              };
                            }
                            {
                              string = {
                                text = "󰂉";
                              };
                            }
                            {
                              string = {
                                text = "󰢞";
                              };
                            }
                            {
                              string = {
                                text = "󰂊";
                              };
                            }
                            {
                              string = {
                                text = "󰂋";
                              };
                            }
                            {
                              string = {
                                text = "󰂅";
                              };
                            }
                          ];
                        };
                      }
                      {
                        string = {
                          text = " {capacity:02}%";
                        };
                      }
                    ];
                    "state == full" = [
                      {
                        string = {
                          text = "󰁹";
                          foreground = "76946Aff";
                        };
                      }
                      {
                        string = {
                          text = " {capacity:02}% full";
                        };
                      }
                    ];
                  };
                };
              };
            };
          }
        ];
      };
    };
  };
}

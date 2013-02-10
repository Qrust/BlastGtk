module GtkBlast.BoardSettingsGuiXML where
import Import
import BlastItWithPiss.Board
import GtkBlast.Directory

boardSettingsGuiXML :: Board -> String
boardSettingsGuiXML board =
 "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
<interface>\
  <requires lib=\"gtk+\" version=\"2.18\"/>\
  <!-- interface-naming-policy project-wide -->\
  <object class=\"GtkAdjustment\" id=\"adjustment1\">\
    <property name=\"upper\">1000000000</property>\
    <property name=\"step_increment\">1</property>\
    <property name=\"page_increment\">10</property>\
  </object>\
  <object class=\"GtkAdjustment\" id=\"adjustment2\">\
    <property name=\"upper\">1000000000</property>\
    <property name=\"step_increment\">1</property>\
    <property name=\"page_increment\">10</property>\
  </object>\
  <object class=\"GtkAdjustment\" id=\"adjustment3\">\
    <property name=\"upper\">1000000000</property>\
    <property name=\"step_increment\">1</property>\
    <property name=\"page_increment\">10</property>\
  </object>\
  <object class=\"GtkWindow\" id=\"window1\">\
    <property name=\"can_focus\">False</property>\
    <property name=\"title\" translatable=\"yes\">Настройки " ++ renderBoard board ++ "</property>\
    <property name=\"role\">wipe-board-settings-" ++ renderBoard board ++ "</property>\
    <property name=\"icon\">" ++ bundledFile "resources/2ch.so.png" ++ "</property>\
    <property name=\"skip_taskbar_hint\">True</property>\
    <child>\
      <object class=\"GtkAlignment\" id=\"alignment1\">\
        <property name=\"visible\">True</property>\
        <property name=\"can_focus\">False</property>\
        <property name=\"top_padding\">3</property>\
        <property name=\"bottom_padding\">3</property>\
        <property name=\"left_padding\">3</property>\
        <property name=\"right_padding\">3</property>\
        <child>\
          <object class=\"GtkVBox\" id=\"vbox1\">\
            <property name=\"visible\">True</property>\
            <property name=\"can_focus\">False</property>\
            <property name=\"spacing\">6</property>\
            <child>\
              <object class=\"GtkVBox\" id=\"vbox2\">\
                <property name=\"visible\">True</property>\
                <property name=\"can_focus\">False</property>\
                <child>\
                  <object class=\"GtkCheckButton\" id=\"checkwipethread\">\
                    <property name=\"label\" translatable=\"yes\">_Вайпать один тред</property>\
                    <property name=\"use_action_appearance\">False</property>\
                    <property name=\"visible\">True</property>\
                    <property name=\"can_focus\">True</property>\
                    <property name=\"receives_default\">False</property>\
                    <property name=\"use_underline\">True</property>\
                    <property name=\"draw_indicator\">True</property>\
                  </object>\
                  <packing>\
                    <property name=\"expand\">True</property>\
                    <property name=\"fill\">True</property>\
                    <property name=\"position\">0</property>\
                  </packing>\
                </child>\
                <child>\
                  <object class=\"GtkAlignment\" id=\"alignmentthread\">\
                    <property name=\"visible\">True</property>\
                    <property name=\"can_focus\">False</property>\
                    <property name=\"left_padding\">10</property>\
                    <child>\
                      <object class=\"GtkVBox\" id=\"vbox4\">\
                        <property name=\"visible\">True</property>\
                        <property name=\"can_focus\">False</property>\
                        <child>\
                          <object class=\"GtkLabel\" id=\"label1\">\
                            <property name=\"visible\">True</property>\
                            <property name=\"can_focus\">False</property>\
                            <property name=\"xalign\">0</property>\
                            <property name=\"label\" translatable=\"yes\">_Номер треда:</property>\
                            <property name=\"use_underline\">True</property>\
                            <property name=\"mnemonic_widget\">spinthreadnum</property>\
                          </object>\
                          <packing>\
                            <property name=\"expand\">True</property>\
                            <property name=\"fill\">True</property>\
                            <property name=\"position\">0</property>\
                          </packing>\
                        </child>\
                        <child>\
                          <object class=\"GtkAlignment\" id=\"alignment3\">\
                            <property name=\"visible\">True</property>\
                            <property name=\"can_focus\">False</property>\
                            <property name=\"left_padding\">10</property>\
                            <child>\
                              <object class=\"GtkSpinButton\" id=\"spinthreadnum\">\
                                <property name=\"visible\">True</property>\
                                <property name=\"can_focus\">True</property>\
                                <property name=\"invisible_char\">●</property>\
                                <property name=\"xalign\">1</property>\
                                <property name=\"invisible_char_set\">True</property>\
                                <property name=\"primary_icon_activatable\">False</property>\
                                <property name=\"secondary_icon_activatable\">False</property>\
                                <property name=\"primary_icon_sensitive\">True</property>\
                                <property name=\"secondary_icon_sensitive\">True</property>\
                                <property name=\"adjustment\">adjustment1</property>\
                                <property name=\"climb_rate\">1</property>\
                                <property name=\"snap_to_ticks\">True</property>\
                                <property name=\"numeric\">True</property>\
                                <property name=\"update_policy\">if-valid</property>\
                              </object>\
                            </child>\
                          </object>\
                          <packing>\
                            <property name=\"expand\">True</property>\
                            <property name=\"fill\">True</property>\
                            <property name=\"position\">1</property>\
                          </packing>\
                        </child>\
                        <child>\
                          <object class=\"GtkCheckButton\" id=\"checksage\">\
                            <property name=\"label\" translatable=\"yes\">_Сажа</property>\
                            <property name=\"use_action_appearance\">False</property>\
                            <property name=\"visible\">True</property>\
                            <property name=\"can_focus\">True</property>\
                            <property name=\"receives_default\">False</property>\
                            <property name=\"use_underline\">True</property>\
                            <property name=\"draw_indicator\">True</property>\
                          </object>\
                          <packing>\
                            <property name=\"expand\">True</property>\
                            <property name=\"fill\">True</property>\
                            <property name=\"position\">2</property>\
                          </packing>\
                        </child>\
                      </object>\
                    </child>\
                  </object>\
                  <packing>\
                    <property name=\"expand\">True</property>\
                    <property name=\"fill\">True</property>\
                    <property name=\"position\">1</property>\
                  </packing>\
                </child>\
              </object>\
              <packing>\
                <property name=\"expand\">False</property>\
                <property name=\"fill\">True</property>\
                <property name=\"position\">0</property>\
              </packing>\
            </child>\
            <child>\
              <object class=\"GtkVBox\" id=\"vbox28\">\
                <property name=\"visible\">True</property>\
                <property name=\"can_focus\">False</property>\
                <child>\
                  <object class=\"GtkCheckButton\" id=\"checkposttimeout\">\
                    <property name=\"label\" translatable=\"yes\">Перерыв между _постами</property>\
                    <property name=\"use_action_appearance\">False</property>\
                    <property name=\"visible\">True</property>\
                    <property name=\"can_focus\">True</property>\
                    <property name=\"receives_default\">False</property>\
                    <property name=\"use_underline\">True</property>\
                    <property name=\"draw_indicator\">True</property>\
                  </object>\
                  <packing>\
                    <property name=\"expand\">False</property>\
                    <property name=\"fill\">False</property>\
                    <property name=\"position\">0</property>\
                  </packing>\
                </child>\
                <child>\
                  <object class=\"GtkAlignment\" id=\"alignment10\">\
                    <property name=\"visible\">True</property>\
                    <property name=\"can_focus\">False</property>\
                    <property name=\"left_padding\">10</property>\
                    <child>\
                      <object class=\"GtkSpinButton\" id=\"spinposttimeout\">\
                        <property name=\"visible\">True</property>\
                        <property name=\"can_focus\">True</property>\
                        <property name=\"invisible_char\">●</property>\
                        <property name=\"xalign\">1</property>\
                        <property name=\"invisible_char_set\">True</property>\
                        <property name=\"primary_icon_activatable\">False</property>\
                        <property name=\"secondary_icon_activatable\">False</property>\
                        <property name=\"primary_icon_sensitive\">True</property>\
                        <property name=\"secondary_icon_sensitive\">True</property>\
                        <property name=\"adjustment\">adjustment3</property>\
                        <property name=\"climb_rate\">1</property>\
                        <property name=\"digits\">2</property>\
                        <property name=\"numeric\">True</property>\
                        <property name=\"update_policy\">if-valid</property>\
                      </object>\
                    </child>\
                  </object>\
                  <packing>\
                    <property name=\"expand\">False</property>\
                    <property name=\"fill\">False</property>\
                    <property name=\"position\">1</property>\
                  </packing>\
                </child>\
              </object>\
              <packing>\
                <property name=\"expand\">False</property>\
                <property name=\"fill\">False</property>\
                <property name=\"position\">1</property>\
              </packing>\
            </child>\
            <child>\
              <object class=\"GtkVBox\" id=\"vbox29\">\
                <property name=\"visible\">True</property>\
                <property name=\"can_focus\">False</property>\
                <child>\
                  <object class=\"GtkCheckButton\" id=\"checkthreadtimeout\">\
                    <property name=\"label\" translatable=\"yes\">Перерыв между _тредами</property>\
                    <property name=\"use_action_appearance\">False</property>\
                    <property name=\"visible\">True</property>\
                    <property name=\"can_focus\">True</property>\
                    <property name=\"receives_default\">False</property>\
                    <property name=\"use_underline\">True</property>\
                    <property name=\"draw_indicator\">True</property>\
                  </object>\
                  <packing>\
                    <property name=\"expand\">False</property>\
                    <property name=\"fill\">False</property>\
                    <property name=\"position\">0</property>\
                  </packing>\
                </child>\
                <child>\
                  <object class=\"GtkAlignment\" id=\"alignment11\">\
                    <property name=\"visible\">True</property>\
                    <property name=\"can_focus\">False</property>\
                    <property name=\"left_padding\">10</property>\
                    <child>\
                      <object class=\"GtkSpinButton\" id=\"spinthreadtimeout\">\
                        <property name=\"visible\">True</property>\
                        <property name=\"can_focus\">True</property>\
                        <property name=\"invisible_char\">●</property>\
                        <property name=\"xalign\">1</property>\
                        <property name=\"invisible_char_set\">True</property>\
                        <property name=\"primary_icon_activatable\">False</property>\
                        <property name=\"secondary_icon_activatable\">False</property>\
                        <property name=\"primary_icon_sensitive\">True</property>\
                        <property name=\"secondary_icon_sensitive\">True</property>\
                        <property name=\"adjustment\">adjustment2</property>\
                        <property name=\"climb_rate\">1</property>\
                        <property name=\"digits\">2</property>\
                        <property name=\"numeric\">True</property>\
                        <property name=\"update_policy\">if-valid</property>\
                      </object>\
                    </child>\
                  </object>\
                  <packing>\
                    <property name=\"expand\">False</property>\
                    <property name=\"fill\">False</property>\
                    <property name=\"position\">1</property>\
                  </packing>\
                </child>\
              </object>\
              <packing>\
                <property name=\"expand\">False</property>\
                <property name=\"fill\">False</property>\
                <property name=\"position\">2</property>\
              </packing>\
            </child>\
            <child>\
              <object class=\"GtkHButtonBox\" id=\"hbuttonbox1\">\
                <property name=\"visible\">True</property>\
                <property name=\"can_focus\">False</property>\
                <property name=\"layout_style\">end</property>\
                <child>\
                  <object class=\"GtkButton\" id=\"buttonapply\">\
                    <property name=\"label\">gtk-apply</property>\
                    <property name=\"use_action_appearance\">False</property>\
                    <property name=\"visible\">True</property>\
                    <property name=\"can_focus\">True</property>\
                    <property name=\"receives_default\">True</property>\
                    <property name=\"use_stock\">True</property>\
                  </object>\
                  <packing>\
                    <property name=\"expand\">False</property>\
                    <property name=\"fill\">False</property>\
                    <property name=\"position\">0</property>\
                  </packing>\
                </child>\
                <child>\
                  <object class=\"GtkButton\" id=\"buttoncancel\">\
                    <property name=\"label\">gtk-cancel</property>\
                    <property name=\"use_action_appearance\">False</property>\
                    <property name=\"visible\">True</property>\
                    <property name=\"can_focus\">True</property>\
                    <property name=\"receives_default\">True</property>\
                    <property name=\"use_stock\">True</property>\
                  </object>\
                  <packing>\
                    <property name=\"expand\">False</property>\
                    <property name=\"fill\">False</property>\
                    <property name=\"position\">1</property>\
                  </packing>\
                </child>\
                <child>\
                  <object class=\"GtkButton\" id=\"buttonok\">\
                    <property name=\"label\">gtk-ok</property>\
                    <property name=\"use_action_appearance\">False</property>\
                    <property name=\"visible\">True</property>\
                    <property name=\"can_focus\">True</property>\
                    <property name=\"receives_default\">True</property>\
                    <property name=\"use_stock\">True</property>\
                  </object>\
                  <packing>\
                    <property name=\"expand\">False</property>\
                    <property name=\"fill\">False</property>\
                    <property name=\"position\">2</property>\
                  </packing>\
                </child>\
              </object>\
              <packing>\
                <property name=\"expand\">False</property>\
                <property name=\"fill\">False</property>\
                <property name=\"position\">3</property>\
              </packing>\
            </child>\
          </object>\
        </child>\
      </object>\
    </child>\
  </object>\
</interface>"

module Updater.GuiXML where
import Prelude

-- FIXME We don't read file using template haskell because of linking errors on windos
guiXML :: String
guiXML = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
<interface>\
  <requires lib=\"gtk+\" version=\"2.18\"/>\
  <!-- interface-naming-policy project-wide -->\
  <object class=\"GtkWindow\" id=\"changelogwindow\">\
    <property name=\"can_focus\">False</property>\
    <property name=\"title\" translatable=\"yes\">Изменения с последней версии</property>\
    <property name=\"role\">blast-it-with-piss-changelog</property>\
    <property name=\"window_position\">center</property>\
    <property name=\"default_width\">475</property>\
    <property name=\"default_height\">250</property>\
    <property name=\"destroy_with_parent\">True</property>\
    <property name=\"icon_name\">dialog-information</property>\
    <property name=\"type_hint\">dialog</property>\
    <property name=\"skip_taskbar_hint\">True</property>\
    <property name=\"transient_for\">updaterwindow</property>\
    <child>\
      <object class=\"GtkVBox\" id=\"vbox2\">\
        <property name=\"visible\">True</property>\
        <property name=\"can_focus\">False</property>\
        <child>\
          <object class=\"GtkAlignment\" id=\"alignment2\">\
            <property name=\"visible\">True</property>\
            <property name=\"can_focus\">False</property>\
            <property name=\"top_padding\">3</property>\
            <property name=\"bottom_padding\">3</property>\
            <property name=\"left_padding\">3</property>\
            <property name=\"right_padding\">3</property>\
            <child>\
              <object class=\"GtkHBox\" id=\"hbox1\">\
                <property name=\"visible\">True</property>\
                <property name=\"can_focus\">False</property>\
                <child>\
                  <object class=\"GtkImage\" id=\"image1\">\
                    <property name=\"visible\">True</property>\
                    <property name=\"can_focus\">False</property>\
                    <property name=\"yalign\">0</property>\
                    <property name=\"stock\">gtk-dialog-info</property>\
                    <property name=\"icon-size\">6</property>\
                  </object>\
                  <packing>\
                    <property name=\"expand\">False</property>\
                    <property name=\"fill\">False</property>\
                    <property name=\"position\">0</property>\
                  </packing>\
                </child>\
                <child>\
                  <object class=\"GtkAlignment\" id=\"alignment3\">\
                    <property name=\"visible\">True</property>\
                    <property name=\"can_focus\">False</property>\
                    <property name=\"left_padding\">3</property>\
                    <child>\
                      <object class=\"GtkScrolledWindow\" id=\"scrolledwindow1\">\
                        <property name=\"visible\">True</property>\
                        <property name=\"can_focus\">True</property>\
                        <property name=\"hscrollbar_policy\">automatic</property>\
                        <property name=\"vscrollbar_policy\">automatic</property>\
                        <child>\
                          <object class=\"GtkViewport\" id=\"viewport1\">\
                            <property name=\"visible\">True</property>\
                            <property name=\"can_focus\">False</property>\
                            <property name=\"shadow_type\">none</property>\
                            <child>\
                              <object class=\"GtkLabel\" id=\"labelchangelog\">\
                                <property name=\"visible\">True</property>\
                                <property name=\"can_focus\">False</property>\
                                <property name=\"xalign\">0</property>\
                                <property name=\"yalign\">0</property>\
                                <property name=\"label\" translatable=\"yes\">&lt;b&gt;0.0.0.0:&lt;/b&gt; Если вы это видите, то вы соснули хуйца.</property>\
                                <property name=\"use_markup\">True</property>\
                                <property name=\"wrap\">True</property>\
                                <property name=\"selectable\">True</property>\
                              </object>\
                            </child>\
                          </object>\
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
            </child>\
          </object>\
          <packing>\
            <property name=\"expand\">True</property>\
            <property name=\"fill\">True</property>\
            <property name=\"position\">0</property>\
          </packing>\
        </child>\
        <child>\
          <object class=\"GtkHButtonBox\" id=\"hbuttonbox1\">\
            <property name=\"visible\">True</property>\
            <property name=\"can_focus\">False</property>\
            <child>\
              <object class=\"GtkButton\" id=\"buttonchangelog\">\
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
                <property name=\"position\">0</property>\
              </packing>\
            </child>\
          </object>\
          <packing>\
            <property name=\"expand\">False</property>\
            <property name=\"fill\">False</property>\
            <property name=\"position\">1</property>\
          </packing>\
        </child>\
      </object>\
    </child>\
  </object>\
  <object class=\"GtkWindow\" id=\"errorwindow\">\
    <property name=\"can_focus\">False</property>\
    <property name=\"title\" translatable=\"yes\">Не удалось обновиться</property>\
    <property name=\"role\">blast-it-with-piss-error</property>\
    <property name=\"window_position\">center</property>\
    <property name=\"default_width\">475</property>\
    <property name=\"default_height\">250</property>\
    <property name=\"destroy_with_parent\">True</property>\
    <property name=\"icon_name\">dialog-error</property>\
    <property name=\"type_hint\">dialog</property>\
    <property name=\"skip_taskbar_hint\">True</property>\
    <property name=\"urgency_hint\">True</property>\
    <property name=\"transient_for\">updaterwindow</property>\
    <child>\
      <object class=\"GtkVBox\" id=\"vbox3\">\
        <property name=\"visible\">True</property>\
        <property name=\"can_focus\">False</property>\
        <child>\
          <object class=\"GtkAlignment\" id=\"alignment4\">\
            <property name=\"visible\">True</property>\
            <property name=\"can_focus\">False</property>\
            <property name=\"top_padding\">3</property>\
            <property name=\"bottom_padding\">3</property>\
            <property name=\"left_padding\">3</property>\
            <property name=\"right_padding\">3</property>\
            <child>\
              <object class=\"GtkHBox\" id=\"hbox2\">\
                <property name=\"visible\">True</property>\
                <property name=\"can_focus\">False</property>\
                <child>\
                  <object class=\"GtkImage\" id=\"image3\">\
                    <property name=\"visible\">True</property>\
                    <property name=\"can_focus\">False</property>\
                    <property name=\"yalign\">0</property>\
                    <property name=\"stock\">gtk-dialog-error</property>\
                    <property name=\"icon-size\">6</property>\
                  </object>\
                  <packing>\
                    <property name=\"expand\">False</property>\
                    <property name=\"fill\">False</property>\
                    <property name=\"position\">0</property>\
                  </packing>\
                </child>\
                <child>\
                  <object class=\"GtkAlignment\" id=\"alignment5\">\
                    <property name=\"visible\">True</property>\
                    <property name=\"can_focus\">False</property>\
                    <property name=\"left_padding\">3</property>\
                    <child>\
                      <object class=\"GtkScrolledWindow\" id=\"scrolledwindow2\">\
                        <property name=\"visible\">True</property>\
                        <property name=\"can_focus\">True</property>\
                        <property name=\"hscrollbar_policy\">automatic</property>\
                        <property name=\"vscrollbar_policy\">automatic</property>\
                        <child>\
                          <object class=\"GtkViewport\" id=\"viewport2\">\
                            <property name=\"visible\">True</property>\
                            <property name=\"can_focus\">False</property>\
                            <property name=\"shadow_type\">none</property>\
                            <child>\
                              <object class=\"GtkLabel\" id=\"labelerror\">\
                                <property name=\"visible\">True</property>\
                                <property name=\"can_focus\">False</property>\
                                <property name=\"xalign\">0</property>\
                                <property name=\"yalign\">0</property>\
                                <property name=\"label\" translatable=\"yes\">Произошла неведомая ёбаная хуйня</property>\
                                <property name=\"use_markup\">True</property>\
                                <property name=\"wrap\">True</property>\
                                <property name=\"selectable\">True</property>\
                              </object>\
                            </child>\
                          </object>\
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
            </child>\
          </object>\
          <packing>\
            <property name=\"expand\">True</property>\
            <property name=\"fill\">True</property>\
            <property name=\"position\">0</property>\
          </packing>\
        </child>\
        <child>\
          <object class=\"GtkHButtonBox\" id=\"hbuttonbox2\">\
            <property name=\"visible\">True</property>\
            <property name=\"can_focus\">False</property>\
            <child>\
              <object class=\"GtkButton\" id=\"buttonerror\">\
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
                <property name=\"position\">0</property>\
              </packing>\
            </child>\
          </object>\
          <packing>\
            <property name=\"expand\">False</property>\
            <property name=\"fill\">False</property>\
            <property name=\"position\">1</property>\
          </packing>\
        </child>\
      </object>\
    </child>\
  </object>\
  <object class=\"GtkWindow\" id=\"updaterwindow\">\
    <property name=\"width_request\">330</property>\
    <property name=\"height_request\">65</property>\
    <property name=\"can_focus\">False</property>\
    <property name=\"title\" translatable=\"yes\">Обновление вайпалки</property>\
    <property name=\"role\">blast-it-with-piss-updater</property>\
    <property name=\"window_position\">center</property>\
    <property name=\"type_hint\">splashscreen</property>\
    <child>\
      <object class=\"GtkVBox\" id=\"vbox1\">\
        <property name=\"visible\">True</property>\
        <property name=\"can_focus\">False</property>\
        <child>\
          <object class=\"GtkLabel\" id=\"labelupdatemessage\">\
            <property name=\"height_request\">25</property>\
            <property name=\"visible\">True</property>\
            <property name=\"can_focus\">False</property>\
            <property name=\"label\" translatable=\"yes\">Если вы это видите то PSSHHHSHSHHSSHSH</property>\
          </object>\
          <packing>\
            <property name=\"expand\">False</property>\
            <property name=\"fill\">True</property>\
            <property name=\"position\">0</property>\
          </packing>\
        </child>\
        <child>\
          <object class=\"GtkAlignment\" id=\"alignment1\">\
            <property name=\"visible\">True</property>\
            <property name=\"can_focus\">False</property>\
            <property name=\"xscale\">0.94999998807907104</property>\
            <property name=\"yscale\">0.94999998807907104</property>\
            <child>\
              <object class=\"GtkProgressBar\" id=\"progressupdate\">\
                <property name=\"height_request\">35</property>\
                <property name=\"visible\">True</property>\
                <property name=\"can_focus\">False</property>\
                <property name=\"activity_mode\">True</property>\
                <property name=\"pulse_step\">0.050000000000000003</property>\
              </object>\
            </child>\
          </object>\
          <packing>\
            <property name=\"expand\">False</property>\
            <property name=\"fill\">True</property>\
            <property name=\"position\">1</property>\
          </packing>\
        </child>\
      </object>\
    </child>\
  </object>\
</interface>"

class InfoTagHandler(GPS.InlineTagHandler):

   def __init__(self):
      super(InfoTagHandler, self).__init__('info')

   def has_parameter(self):
      return True

   def to_markup(self, writer, parameter):
      writer.generate_inline()
      writer.text(parameter)

GPS.register_tag_handler(InfoTagHandler())

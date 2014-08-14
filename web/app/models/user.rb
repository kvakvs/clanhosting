class User < ActiveRecord::Base
  def forem_admin?
    false if self.nil?
    forem_admin
  end
end

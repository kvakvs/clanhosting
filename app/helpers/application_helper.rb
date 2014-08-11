module ApplicationHelper
  def user_signed_in?
    session.has_key? :user_account
  end
end

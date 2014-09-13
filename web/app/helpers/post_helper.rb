module PostHelper
  def quote(text)
    text.lines.map { |ln|
      "> #{ln}"
    }.join("\n")
  end

  def get_username(user_id)
    rpc = Rails.application.get_rpc
    rpc.call.ch_user_api.get_username(Integer(user_id),
                                      session[:user_token]) || "User#{user_id}"
  end
end

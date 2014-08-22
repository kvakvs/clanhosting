class HomeController < ApplicationController
  def index
  end

  def wot_logged_in
    if params['status'] == 'ok'
      Rails.logger.info(request.env)
      # TODO: Custom Erlang-based session store
      session[:user_nickname]      = params['nickname']
      session[:user_account]       = Integer(params['account_id'])
      session[:user_token] = token = params['access_token']
      session[:user_expires_at]    = Integer(params['expires_at']) + Time.now.to_i

      # Check user_token
      acc_id = Integer(params['account_id'])
      rpc = Rails.application.get_rpc
      acc_info = rpc.call.ch_user_api.after_login(acc_id, token, 'en')
      session[:account_info] = acc_info
      session[:user_clan] = acc_info['clan_id']
      fetch_clan_info

      redirect_to root_path
    end
  end

  def wot_log_out
    rpc = Rails.application.get_rpc
    rpc.call.ch_user_api.logged_out(session[:user_account])
    session.clear
  end

  def new_forum

  end

  def fetch_clan_info
    rpc = Rails.application.get_rpc
    clan_info = rpc.call.ch_clan_api.clan_info(session[:user_clan],
                                               session[:user_token],
                                               'en')
    clan_info['name'] = clan_info['name'].force_encoding('utf-8')
    clan_info['abbreviation'] = clan_info['abbreviation'].force_encoding('utf-8')

    session[:clan_info] = clan_info
    session[:clan_forum_exists] = Forem::Category.exists?(:name => session[:user_clan].to_s)

    m = clan_info['members'][session[:user_account].to_s]
    session[:is_clan_admin] = (not m.nil? and m['role'] == 'leader')
  end
end
